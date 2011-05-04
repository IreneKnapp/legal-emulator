//
//  GameView.m
//  Legal Emulator
//
//  Created by Dan Knapp on 4/30/11.
//  Copyright 2011 Dan Knapp. All rights reserved.
//

#import "GameView.h"
#import "Emulator.h"


@implementation GameView

- (void) awakeFromNib {
    initialized = NO;
    traceExecution = YES;
}


- (void) prepareGameFromFilename: (NSString *) filename {
    game = emulator_load_game((char *) [filename UTF8String]);
    gamestate = game_power_on_state(game);
    char *trace = NULL;
    void *next_gamestate
      = gamestate_frame_forward(gamestate, traceExecution ? trace : NULL);
    if(trace) {
        printf("%s", trace);
        string_free(trace);
    }
    gamestate_free(gamestate);
    gamestate = next_gamestate;
    
    NSString *windowTitle = @"No Game";
    if(game) {
        char *name = game_name(game);
        windowTitle = [NSString stringWithUTF8String: name];
        string_free(name);
    }
    [[self window] setTitle: windowTitle];
}


- (BOOL) acceptsFirstResponder {
    return YES;
}


- (void) keyDown: (NSEvent *) event {
    switch([event keyCode]) {
    case 0: // a
        if(currentTexture > 1)
            currentTexture -= 2;
        [self setNeedsDisplay: YES];
        break;
    case 1: // s
        if(currentTexture + 2 < nTextures)
            currentTexture += 2;
        [self setNeedsDisplay: YES];
        break;
    /*
    case 2: // d
        w += 0.125f;
        [self setNeedsDisplay: YES];
        break;
    case 13: // w
        h -= 0.125f;
        [self setNeedsDisplay: YES];
        break;
    */
    }
}


- (void) initOpenGL {
    NSOpenGLContext *context = [self openGLContext];
    long swapInterval = 1;
    [context setValues: (const GLint *) &swapInterval
             forParameter: NSOpenGLCPSwapInterval];
    
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glOrtho(0, 256, 240, 0, -1, 1);
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    
    glClearColor(0.0f, 0.0f, 0.0f, 1.0f);
    
    glClearStencil(0x80);
    
    glDepthFunc(GL_LEQUAL);
    glClearDepth(1.0f);
    
    glAlphaFunc(GL_GREATER, 0.0f);
    
    GLchar *shaderSource =
        "uniform float kernel[25];\n"
        "uniform float scale;\n"
        "uniform float contrastAdjustment;\n"
        "uniform sampler2D texture;\n"
        "\n"
        "void main() {\n"
        "    int x, y;\n"
        "    vec4 total = vec4(0.0);\n"
        "    for(y = 0; y < 5; y++) {\n"
        "        for(x = 0; x < 5; x++) {\n"
        "           total += texture2D(texture,\n"
        "                              gl_TexCoord[0].st\n"
        "                              + vec2(float(x - 2)\n"
        "                                     / (256.0 * scale),\n"
        "                                     float(y - 2)\n"
        "                                     / (240.0 * scale)))\n"
        "                    * kernel[y * 5 + x];\n"
        "        }\n"
        "    }\n"
        "    total *= contrastAdjustment;\n"
        "    gl_FragColor = total;\n"
        "}\n";
    shader = glCreateShader(GL_FRAGMENT_SHADER);
    glShaderSource(shader, 1, (const GLchar **) &shaderSource, NULL);
    glCompileShader(shader);
    GLint compileStatus;
    glGetShaderiv(program, GL_COMPILE_STATUS, &compileStatus);
    if(compileStatus == GL_FALSE) {
        GLint infoLogLength;
        glGetShaderiv(shader, GL_INFO_LOG_LENGTH, &infoLogLength);
        GLchar *buffer = malloc(infoLogLength + 1);
        glGetShaderInfoLog(shader, infoLogLength + 1, NULL, buffer);
        printf("%s\n", buffer);
        free(buffer);
    }
    
    program = glCreateProgram();
    glAttachShader(program, shader);
    glLinkProgram(program);
    
    [self recomputeConvolutionKernel];
}


- (void) recomputeConvolutionKernel {
    GLint viewport[4];
    glGetIntegerv(GL_VIEWPORT, viewport);
    
    float scale = ((float) viewport[2]) / 256.0;
    
    GLfloat *convolutionKernelData = malloc(sizeof(GLfloat) * 5 * 5);
    float sigma = powf(0.8f, 4.0f / scale);
    float usefulFactor = 0.5f * powf(sigma, -2.0f);
    float centerValue = usefulFactor / 3.14159f;
    for(int y = 0; y < 5; y++) {
        for(int x = 0; x < 5; x++) {
            int distanceY = y - 2;
            int distanceX = x - 2;
            float expTop = powf(distanceX, 2.0f) + powf(distanceY, 2.0f);
            float value = centerValue * expf(-expTop * usefulFactor);
            float scaledValue = value / centerValue;
            convolutionKernelData[y * 5 + x] = scaledValue;
        }
    }
    
    GLfloat convolutionKernelScale = 0.0f;
    for(int i = 0; i < 25; i++) {
        convolutionKernelScale += convolutionKernelData[i];
    }
    convolutionKernelScale = 1.0f / convolutionKernelScale;
    
    for(int i = 0; i < 25; i++) {
        convolutionKernelData[i] *= convolutionKernelScale;
    }
    
    glUseProgram(program);
    GLint location = glGetUniformLocation(program, "kernel");
    glUniform1fv(location, 25, convolutionKernelData);
    location = glGetUniformLocation(program, "scale");
    glUniform1f(location, scale);
    
    free(convolutionKernelData);
}


- (void) reshape {
    [[self openGLContext] makeCurrentContext];
    
    NSSize size = [self bounds].size;
    glViewport(0, 0, size.width, size.height);
    
    [self recomputeConvolutionKernel];
    
    [self setNeedsDisplay: YES];
}


- (void) initTextures {
    [[self openGLContext] makeCurrentContext];
    
    glEnable(GL_TEXTURE_2D);
    
    uint8_t *buffer = malloc(sizeof(uint8_t) * 16384);
    
    nTextures = game_get_n_textures(game);
    textures = malloc(sizeof(GLuint) * nTextures);
    glGenTextures(nTextures, textures);
    
    for(uint32_t i = 0; i < nTextures; i++) {
        game_get_texture(game, i, buffer);
        
        glBindTexture(GL_TEXTURE_2D, textures[i]);
        glTexImage2D(GL_TEXTURE_2D,
		             0,
                     GL_ALPHA,
                     128, 128,
                     0,
                     GL_ALPHA,
                     GL_UNSIGNED_BYTE,
                     buffer);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
    }
    
    free(buffer);
    
    glGenTextures(1, &temporaryTexture);
    glBindTexture(GL_TEXTURE_2D, temporaryTexture);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
}


- (void) drawRect: (NSRect) dirtyRectangle {
    if(!initialized) {
        [self initOpenGL];
        [self initTextures];
        
        currentTexture = 0;
        
        initialized = YES;
    }
    
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    
    glDisable(GL_COLOR_LOGIC_OP);
    glEnable(GL_ALPHA_TEST);
    glEnable(GL_STENCIL_TEST);
    glEnable(GL_DEPTH_TEST);
    glEnable(GL_TEXTURE_2D);
    glUseProgram(0);
    
    glColor4f(1.0f, 1.0f, 1.0f, 1.0f);
    
    glStencilMask(0xFF);
    glClear(GL_STENCIL_BUFFER_BIT);
    
    glStencilOp(GL_REPLACE, GL_REPLACE, GL_KEEP);
    glDepthFunc(GL_ALWAYS);
    
    glStencilFunc(GL_NEVER, 0x00, 0xFF);
    glBindTexture(GL_TEXTURE_2D, 0);
    /*
    glBegin(GL_QUADS);
    glVertex2s(13, 118);
    glVertex2s(256, 118);
    glVertex2s(256, 119);
    glVertex2s(13, 119);
    glVertex2s(0, 119);
    glVertex2s(256, 119);
    glVertex2s(256, 240);
    glVertex2s(0, 240);
    glEnd();
    */
    
    glStencilFunc(GL_EQUAL, 0xFF, 0x80);
    glDepthFunc(GL_NEVER);
    
    void *video_frame = gamestate_get_video_frame(gamestate);
    uint8_t *name_table = malloc(sizeof(uint8_t) * 33 * 30);
    video_frame_get_name_table(video_frame, name_table);
    video_frame_free(video_frame);
    
    for(int nameTableY = 0; nameTableY < 30; nameTableY++) {
        for(int nameTableX = 0; nameTableX < 33; nameTableX++) {
            uint8_t name = name_table[nameTableY * 33 + nameTableX];
            int patternTableCellX = name % 16;
            int patternTableCellY = name / 16;
            int patternTablePixelLeft = patternTableCellX * 8;
            int patternTablePixelRight = patternTablePixelLeft + 8;
            int patternTablePixelTop = patternTableCellY * 8;
            int patternTablePixelBottom = patternTablePixelTop + 8;
            float patternTableTextureLeft = patternTablePixelLeft / 128.0;
            float patternTableTextureRight = patternTablePixelRight / 128.0;
            float patternTableTextureTop = patternTablePixelTop / 128.0;
            float patternTableTextureBottom = patternTablePixelBottom / 128.0;
            
            int backgroundCellX = nameTableX;
            int backgroundCellY = nameTableY;
            int backgroundPixelLeft = backgroundCellX * 8;
            int backgroundPixelRight = backgroundPixelLeft + 8;
            int backgroundPixelTop = backgroundCellY * 8;
            int backgroundPixelBottom = backgroundPixelTop + 8;
            
            glStencilMask(0x01);
            glBindTexture(GL_TEXTURE_2D, textures[currentTexture]);
            glBegin(GL_QUADS);
            glTexCoord2f(patternTableTextureLeft, patternTableTextureTop);
            glVertex2s(backgroundPixelLeft, backgroundPixelTop);
            glTexCoord2f(patternTableTextureRight, patternTableTextureTop);
            glVertex2s(backgroundPixelRight, backgroundPixelTop);
            glTexCoord2f(patternTableTextureRight, patternTableTextureBottom);
            glVertex2s(backgroundPixelRight, backgroundPixelBottom);
            glTexCoord2f(patternTableTextureLeft, patternTableTextureBottom);
            glVertex2s(backgroundPixelLeft, backgroundPixelBottom);
            glEnd();
            
            glStencilMask(0x02);
            glBindTexture(GL_TEXTURE_2D, textures[currentTexture+1]);
            glBegin(GL_QUADS);
            glTexCoord2f(patternTableTextureLeft, patternTableTextureTop);
            glVertex2s(backgroundPixelLeft, backgroundPixelTop);
            glTexCoord2f(patternTableTextureRight, patternTableTextureTop);
            glVertex2s(backgroundPixelRight, backgroundPixelTop);
            glTexCoord2f(patternTableTextureRight, patternTableTextureBottom);
            glVertex2s(backgroundPixelRight, backgroundPixelBottom);
            glTexCoord2f(patternTableTextureLeft, patternTableTextureBottom);
            glVertex2s(backgroundPixelLeft, backgroundPixelBottom);
            glEnd();
        }
    }
    
    free(name_table);
    
    glBindTexture(GL_TEXTURE_2D, 0);
    
    glStencilMask(0x04);
    glBegin(GL_QUADS);
    glVertex2s(0, 0);
    glVertex2s(32, 0);
    glVertex2s(32, 32);
    glVertex2s(0, 32);
    glEnd();
    
    glStencilMask(0x08);
    glBegin(GL_QUADS);
    glVertex2s(32, 0);
    glVertex2s(64, 0);
    glVertex2s(64, 32);
    glVertex2s(32, 32);
    glEnd();
    
    glStencilMask(0x0C);
    glBegin(GL_QUADS);
    glVertex2s(0, 32);
    glVertex2s(32, 32);
    glVertex2s(32, 64);
    glVertex2s(0, 64);
    glEnd();
    
    glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);
    glDepthFunc(GL_LEQUAL);
    
    float colors[16][4] =
        {
            { 0.0, 0.0, 0.25, 1.0 },
            { 0.0, 0.0, 0.5, 1.0 },
            { 0.0, 0.0, 0.75, 1.0 },
            { 0.0, 0.0, 1.0, 1.0 },
            { 0.0, 0.25, 0.0, 1.0 },
            { 0.0, 0.5, 0.0, 1.0 },
            { 0.0, 0.75, 0.0, 1.0 },
            { 0.0, 1.0, 0.0, 1.0 },
            { 0.25, 0.0, 0.0, 1.0 },
            { 0.5, 0.0, 0.0, 1.0 },
            { 0.75, 0.0, 0.0, 1.0 },
            { 1.0, 0.0, 0.0, 1.0 },
            { 0.25, 0.25, 0.25, 1.0 },
            { 0.5, 0.5, 0.5, 1.0 },
            { 0.75, 0.75, 0.75, 1.0 },
            { 1.0, 1.0, 1.0, 1.0 }
        };
    for(int i = 0; i < 16; i++) {
        glStencilFunc(GL_EQUAL, 0x80 | i, 0xFF);
        glColor4f(colors[i][0], colors[i][1], colors[i][2], colors[i][3]);
        glBegin(GL_QUADS);
        glVertex3s(0, 0, 0);
        glVertex3s(256, 0, 0);
        glVertex3s(256, 240, 0);
        glVertex3s(0, 240, 0);
        glEnd();
    }
    
    GLint viewport[4];
    glGetIntegerv(GL_VIEWPORT, viewport);
    
    float scale = ((float) viewport[2]) / 256.0;
    
    if(scale >= 2.0) {
        float contrastAdjustment = 2.0;
        
        glStencilFunc(GL_ALWAYS, 0x00, 0x00);
        glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);
        glDepthFunc(GL_ALWAYS);
        
        glEnable(GL_COLOR_LOGIC_OP);
        glLogicOp(GL_AND);
        
        if(scale >= 4.0f) {
            contrastAdjustment *= 2.0;
            
            double interPixelHorizontalSpace = 0.0005;
            for(int x = 0; x < 256; x++) {
                double farLeft = x;
                double farRight = x + 1.0;
                double pixelAStart = x + interPixelHorizontalSpace / 2.0;
                double pixelAEnd = x + 0.5 - interPixelHorizontalSpace / 2.0;
                double pixelBStart = x + 0.5 + interPixelHorizontalSpace / 2.0;
                double pixelBEnd = x + 1.0 - interPixelHorizontalSpace / 2.0;
                double pixelASubpixelBoundaryA
                    = pixelAStart + (pixelAEnd - pixelAStart) * (1.0 / 3.0);
                double pixelASubpixelBoundaryB
                    = pixelAStart + (pixelAEnd - pixelAStart) * (2.0 / 3.0);
                double pixelBSubpixelBoundaryA
                    = pixelBStart + (pixelBEnd - pixelBStart) * (1.0 / 3.0);
                double pixelBSubpixelBoundaryB
                    = pixelBStart + (pixelBEnd - pixelBStart) * (2.0 / 3.0);
                
                glColor4f(0.0f, 0.0f, 0.0f, 1.0f);
                glBegin(GL_QUADS);
                glVertex2f(farLeft, 0);
                glVertex2f(pixelAStart, 0);
                glVertex2f(pixelAStart, 240);
                glVertex2f(farLeft, 240);
                glEnd();
                
                glColor4f(1.0f, 0.0f, 0.0f, 1.0f);
                glBegin(GL_QUADS);
                glVertex2f(pixelAStart, 0);
                glVertex2f(pixelASubpixelBoundaryA, 0);
                glVertex2f(pixelASubpixelBoundaryA, 240);
                glVertex2f(pixelAStart, 240);
                glEnd();
                
                glColor4f(0.0f, 1.0f, 0.0f, 1.0f);
                glBegin(GL_QUADS);
                glVertex2f(pixelASubpixelBoundaryA, 0);
                glVertex2f(pixelASubpixelBoundaryB, 0);
                glVertex2f(pixelASubpixelBoundaryB, 240);
                glVertex2f(pixelASubpixelBoundaryA, 240);
                glEnd();
                
                glColor4f(0.0f, 0.0f, 1.0f, 1.0f);
                glBegin(GL_QUADS);
                glVertex2f(pixelASubpixelBoundaryB, 0);
                glVertex2f(pixelAEnd, 0);
                glVertex2f(pixelAEnd, 240);
                glVertex2f(pixelASubpixelBoundaryB, 240);
                glEnd();
                
                glColor4f(0.0f, 0.0f, 0.0f, 1.0f);
                glBegin(GL_QUADS);
                glVertex2f(pixelAEnd, 0);
                glVertex2f(pixelBStart, 0);
                glVertex2f(pixelBStart, 240);
                glVertex2f(pixelAEnd, 240);
                glEnd();
                
                glColor4f(1.0f, 0.0f, 0.0f, 1.0f);
                glBegin(GL_QUADS);
                glVertex2f(pixelBStart, 0);
                glVertex2f(pixelBSubpixelBoundaryA, 0);
                glVertex2f(pixelBSubpixelBoundaryA, 240);
                glVertex2f(pixelBStart, 240);
                glEnd();
                
                glColor4f(0.0f, 1.0f, 0.0f, 1.0f);
                glBegin(GL_QUADS);
                glVertex2f(pixelBSubpixelBoundaryA, 0);
                glVertex2f(pixelBSubpixelBoundaryB, 0);
                glVertex2f(pixelBSubpixelBoundaryB, 240);
                glVertex2f(pixelBSubpixelBoundaryA, 240);
                glEnd();
                
                glColor4f(0.0f, 0.0f, 1.0f, 1.0f);
                glBegin(GL_QUADS);
                glVertex2f(pixelBSubpixelBoundaryB, 0);
                glVertex2f(pixelBEnd, 0);
                glVertex2f(pixelBEnd, 240);
                glVertex2f(pixelBSubpixelBoundaryB, 240);
                glEnd();
                
                glColor4f(0.0f, 0.0f, 0.0f, 1.0f);
                glBegin(GL_QUADS);
                glVertex2f(pixelBEnd, 0);
                glVertex2f(farRight, 0);
                glVertex2f(farRight, 240);
                glVertex2f(pixelBEnd, 240);
                glEnd();
            }
        }
        
        for(int y = 0; y < 240; y++) {
            double stripeTop = y + 0.5f;
            double stripeBottom = y + 1.0f;
            
            glColor4f(0.0f, 0.0f, 0.0f, 1.0f);
            glBegin(GL_QUADS);
            glVertex2f(0, stripeTop);
            glVertex2f(256, stripeTop);
            glVertex2f(256, stripeBottom);
            glVertex2f(0, stripeBottom);
            glEnd();
        }
        
        glDisable(GL_COLOR_LOGIC_OP);
        glDisable(GL_ALPHA_TEST);
        glDisable(GL_STENCIL_TEST);
        glDisable(GL_DEPTH_TEST);
        glEnable(GL_TEXTURE_2D);
        
        glUseProgram(program);
        GLint location = glGetUniformLocation(program, "contrastAdjustment");
        glUniform1f(location, contrastAdjustment);
        
        glBindTexture(GL_TEXTURE_2D, temporaryTexture);
        
        glCopyTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA,
                         viewport[0], viewport[1], viewport[2], viewport[3],
                         0);
        
        glClear(GL_COLOR_BUFFER_BIT);
        
        glColor4f(1.0f, 1.0f, 1.0f, 1.0f);
        glBegin(GL_QUADS);
        glTexCoord2f(0.0f, 1.0f);
        glVertex2s(0.0f, 0.0f);
        glTexCoord2f(1.0f, 1.0f);
        glVertex2s(256.0f, 0.0f);
        glTexCoord2f(1.0f, 0.0f);
        glVertex2s(256.0f, 240.0f);
        glTexCoord2f(0.0f, 0.0f);
        glVertex2s(0.0f, 240.0f);
        glEnd();
    }
    
    glFlush();
    [[self openGLContext] flushBuffer];
}

@end

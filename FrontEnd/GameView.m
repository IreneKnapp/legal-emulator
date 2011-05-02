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
}


- (void) prepareGameFromFilename: (NSString *) filename {
    game = emulator_load_game((char *) [filename UTF8String]);
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
    
    glClearColor(0.0f, 0.0f, 0.0f, 1.0f);
    glClearStencil(0x80);
    
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glOrtho(0, 256, 240, 0, -1, 1);
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    
    glDepthFunc(GL_LEQUAL);
    glClearDepth(1.0f);
    
    glAlphaFunc(GL_GREATER, 0.0f);
    
    glClearAccum(0.0f, 0.0f, 0.0f, 0.0f);
}


- (void) reshape {
    [[self openGLContext] makeCurrentContext];
    NSSize size = [self bounds].size;
    glViewport(0, 0, size.width, size.height);
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
    glDisable(GL_BLEND);
    glDisable(GL_CONVOLUTION_2D);
    glEnable(GL_ALPHA_TEST);
    glEnable(GL_STENCIL_TEST);
    glEnable(GL_DEPTH_TEST);
    glEnable(GL_TEXTURE_2D);
    
    glColor4f(1.0f, 1.0f, 1.0f, 1.0f);
    
    glStencilMask(0xFF);
    glClear(GL_STENCIL_BUFFER_BIT);
    
    glStencilOp(GL_REPLACE, GL_REPLACE, GL_KEEP);
    glDepthFunc(GL_ALWAYS);
    
    glStencilFunc(GL_NEVER, 0x00, 0xFF);
    glBindTexture(GL_TEXTURE_2D, 0);
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
    
    glStencilFunc(GL_EQUAL, 0xFF, 0x80);
    glDepthFunc(GL_NEVER);
    
    glStencilMask(0x01);
    glBindTexture(GL_TEXTURE_2D, textures[currentTexture]);
    glBegin(GL_QUADS);
    glTexCoord2f(0.0, 0.0);
    glVertex2s(0, 0);
    glTexCoord2f(1.0, 0.0);
    glVertex2s(128, 0);
    glTexCoord2f(1.0, 1.0);
    glVertex2s(128, 128);
    glTexCoord2f(0.0, 1.0);
    glVertex2s(0, 128);
    glEnd();
    
    glStencilMask(0x02);
    glBindTexture(GL_TEXTURE_2D, textures[currentTexture+1]);
    glBegin(GL_QUADS);
    glTexCoord2f(0.0, 0.0);
    glVertex2s(0, 0);
    glTexCoord2f(1.0, 0.0);
    glVertex2s(128, 0);
    glTexCoord2f(1.0, 1.0);
    glVertex2s(128, 128);
    glTexCoord2f(0.0, 1.0);
    glVertex2s(0, 128);
    glEnd();
    
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
    float contrastAdjustment = 1.0;
    
    if(scale > 1.0) {
        contrastAdjustment *= 2.0;
        
        glStencilFunc(GL_ALWAYS, 0x00, 0x00);
        glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);
        glDepthFunc(GL_ALWAYS);
        
        glDisable(GL_BLEND);
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
                
                glColor4f(0.0, 0.0, 0.0, 1.0);
                glBegin(GL_QUADS);
                glVertex2f(farLeft, 0);
                glVertex2f(pixelAStart, 0);
                glVertex2f(pixelAStart, 240);
                glVertex2f(farLeft, 240);
                glEnd();
                
                glColor4f(1.0, 0.0, 0.0, 1.0);
                glBegin(GL_QUADS);
                glVertex2f(pixelAStart, 0);
                glVertex2f(pixelASubpixelBoundaryA, 0);
                glVertex2f(pixelASubpixelBoundaryA, 240);
                glVertex2f(pixelAStart, 240);
                glEnd();
                
                glColor4f(0.0, 1.0, 0.0, 1.0);
                glBegin(GL_QUADS);
                glVertex2f(pixelASubpixelBoundaryA, 0);
                glVertex2f(pixelASubpixelBoundaryB, 0);
                glVertex2f(pixelASubpixelBoundaryB, 240);
                glVertex2f(pixelASubpixelBoundaryA, 240);
                glEnd();
                
                glColor4f(0.0, 0.0, 1.0, 1.0);
                glBegin(GL_QUADS);
                glVertex2f(pixelASubpixelBoundaryB, 0);
                glVertex2f(pixelAEnd, 0);
                glVertex2f(pixelAEnd, 240);
                glVertex2f(pixelASubpixelBoundaryB, 240);
                glEnd();
                
                glColor4f(0.0, 0.0, 0.0, 1.0);
                glBegin(GL_QUADS);
                glVertex2f(pixelAEnd, 0);
                glVertex2f(pixelBStart, 0);
                glVertex2f(pixelBStart, 240);
                glVertex2f(pixelAEnd, 240);
                glEnd();
                
                glColor4f(1.0, 0.0, 0.0, 1.0);
                glBegin(GL_QUADS);
                glVertex2f(pixelBStart, 0);
                glVertex2f(pixelBSubpixelBoundaryA, 0);
                glVertex2f(pixelBSubpixelBoundaryA, 240);
                glVertex2f(pixelBStart, 240);
                glEnd();
                
                glColor4f(0.0, 1.0, 0.0, 1.0);
                glBegin(GL_QUADS);
                glVertex2f(pixelBSubpixelBoundaryA, 0);
                glVertex2f(pixelBSubpixelBoundaryB, 0);
                glVertex2f(pixelBSubpixelBoundaryB, 240);
                glVertex2f(pixelBSubpixelBoundaryA, 240);
                glEnd();
                
                glColor4f(0.0, 0.0, 1.0, 1.0);
                glBegin(GL_QUADS);
                glVertex2f(pixelBSubpixelBoundaryB, 0);
                glVertex2f(pixelBEnd, 0);
                glVertex2f(pixelBEnd, 240);
                glVertex2f(pixelBSubpixelBoundaryB, 240);
                glEnd();
                
                glColor4f(0.0, 0.0, 0.0, 1.0);
                glBegin(GL_QUADS);
                glVertex2f(pixelBEnd, 0);
                glVertex2f(farRight, 0);
                glVertex2f(farRight, 240);
                glVertex2f(pixelBEnd, 240);
                glEnd();
            }
        }
        
        for(int y = 0; y < 240; y++) {
            double stripeTop = y + 0.5;
            double stripeBottom = y + 1.0;
            
            glColor4f(0.0, 0.0, 0.0, 1.0);
            glBegin(GL_QUADS);
            glVertex2f(0, stripeTop);
            glVertex2f(256, stripeTop);
            glVertex2f(256, stripeBottom);
            glVertex2f(0, stripeBottom);
            glEnd();
        }
            
        glDisable(GL_COLOR_LOGIC_OP);
        glDisable(GL_TEXTURE_2D);
        glDisable(GL_BLEND);
        glDisable(GL_ALPHA_TEST);
        glDisable(GL_STENCIL_TEST);
        glDisable(GL_DEPTH_TEST);
        glEnable(GL_CONVOLUTION_2D);
        int convolutionFilterWidth = 5;
        int convolutionFilterHeight = 5;
        uint8_t *convolutionFilterData
            = malloc(convolutionFilterWidth * convolutionFilterHeight * 4);
        float sigma = powf(0.8f, scale / 4.0f);
        float usefulFactor = 0.5 * powf(sigma, -2.0f);
        float centerValue = usefulFactor / 3.14159f;
        for(int y = 0; y < convolutionFilterHeight; y++) {
            for(int x = 0; x < convolutionFilterWidth; x++) {
                int distanceX = x - convolutionFilterWidth / 2;
                int distanceY = y - convolutionFilterHeight / 2;
                float expTop = powf(distanceX, 2.0) + powf(distanceY, 2.0);
                float value = centerValue * expf(-expTop * usefulFactor);
                uint8_t scaledValue = value / centerValue * 255.0;
                for(int i = 0; i < 4; i++)
                    convolutionFilterData[(y * convolutionFilterWidth + x) * 4 + i]
                        = scaledValue;
            }
        }
        
        GLfloat convolutionFilterScale = 0.0;
        for(int y = 0; y < convolutionFilterHeight; y++) {
            for(int x = 0; x < convolutionFilterWidth; x++) {
                int baseValueIndex = (y * convolutionFilterWidth + x) * 4;
                uint8_t redValue = convolutionFilterData[baseValueIndex + 0];
                uint8_t greenValue = convolutionFilterData[baseValueIndex + 1];
                uint8_t blueValue = convolutionFilterData[baseValueIndex + 2];
                uint8_t alphaValue = convolutionFilterData[baseValueIndex + 3];
                uint8_t maxValue = 0;
                if(maxValue < redValue) maxValue = redValue;
                if(maxValue < greenValue) maxValue = greenValue;
                if(maxValue < blueValue) maxValue = blueValue;
                if(maxValue < alphaValue) maxValue = alphaValue;
                convolutionFilterScale += maxValue;
            }
        }
        convolutionFilterScale = 255.0 / convolutionFilterScale;
        
        glConvolutionParameteri(GL_CONVOLUTION_2D,
                                GL_CONVOLUTION_BORDER_MODE,
                                GL_CONSTANT_BORDER);
        GLfloat convolutionBorderColor[4] = { 0.0f, 0.0f, 0.0f, 1.0f };
        glConvolutionParameterfv(GL_CONVOLUTION_2D,
                                 GL_CONVOLUTION_BORDER_COLOR,
                                 convolutionBorderColor);
        GLfloat convolutionFilterScaleColor[4] =
            {
                convolutionFilterScale,
                convolutionFilterScale,
                convolutionFilterScale,
                convolutionFilterScale
            };
        GLfloat convolutionFilterBiasColor[4] =
            {
                0.0,
                0.0,
                0.0,
                0.0
            };
        glConvolutionParameterfv(GL_CONVOLUTION_2D,
                                 GL_CONVOLUTION_FILTER_SCALE,
                                 convolutionFilterScaleColor);
        glConvolutionParameterfv(GL_CONVOLUTION_2D,
                                 GL_CONVOLUTION_FILTER_BIAS,
                                 convolutionFilterBiasColor);
        glConvolutionFilter2D(GL_CONVOLUTION_2D,
                              GL_RGBA,
                              convolutionFilterWidth,
                              convolutionFilterHeight,
                              GL_RGBA,
                              GL_UNSIGNED_BYTE,
                              convolutionFilterData);
        free(convolutionFilterData);
        glRasterPos2i(0, 240);
        glPixelTransferf(GL_POST_CONVOLUTION_RED_SCALE, contrastAdjustment);
        glPixelTransferf(GL_POST_CONVOLUTION_GREEN_SCALE, contrastAdjustment);
        glPixelTransferf(GL_POST_CONVOLUTION_BLUE_SCALE, contrastAdjustment);
        glPixelTransferf(GL_POST_CONVOLUTION_ALPHA_SCALE, contrastAdjustment);
        glPixelTransferf(GL_POST_CONVOLUTION_RED_BIAS, 0.0f);
        glPixelTransferf(GL_POST_CONVOLUTION_GREEN_BIAS, 0.0f);
        glPixelTransferf(GL_POST_CONVOLUTION_BLUE_BIAS, 0.0f);
        glPixelTransferf(GL_POST_CONVOLUTION_ALPHA_BIAS, 0.0f);
        glCopyPixels(viewport[0],
                     viewport[1],
                     viewport[2],
                     viewport[3],
                     GL_COLOR);
    }
    
    glFlush();
    [[self openGLContext] flushBuffer];
}

@end

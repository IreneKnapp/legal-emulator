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
    
    glEnable(GL_DEPTH_TEST);
    glDepthFunc(GL_LEQUAL);
    glClearDepth(1.0f);
    
    glEnable(GL_ALPHA_TEST);
    glAlphaFunc(GL_GREATER, 0.0f);
    
    glEnable(GL_STENCIL_TEST);
    
    glEnable(GL_TEXTURE_2D);
    
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
    
    /*
    glGenTextures(1, &pixelTexture);
    
    uint8_t pixelTextureBuffer[64] =
        {
            0x80, 0x40, 0x40, 0x00, 0x00, 0x40, 0x40, 0x80,
            0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x40,
            0x20, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x20, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80,
            0x20, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80,
            0x20, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x20, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80
        };
    glBindTexture(GL_TEXTURE_2D, pixelTexture);
    glTexImage2D(GL_TEXTURE_2D,
                 0,
                 GL_ALPHA,
                 8, 8,
                 0,
                 GL_ALPHA,
                 GL_UNSIGNED_BYTE,
                 pixelTextureBuffer);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
    */
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
    
    /*
    glStencilFunc(GL_ALWAYS, 0x00, 0x00);
    glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);
    glDepthFunc(GL_ALWAYS);
    
    glColor4f(1.0f, 1.0f, 1.0f, 1.0f);
    glBindTexture(GL_TEXTURE_2D, pixelTexture);
    glEnable(GL_BLEND);
    glBlendEquation(GL_FUNC_REVERSE_SUBTRACT);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glBegin(GL_QUADS);
    glTexCoord2f(0.0f, 0.0f);
    glVertex2s(0, 0);
    glTexCoord2f(256.0f, 0.0f);
    glVertex2s(256, 0);
    glTexCoord2f(256.0f, 240.0f);
    glVertex2s(256, 240);
    glTexCoord2f(0.0f, 240.0f);
    glVertex2s(0, 240);
    glEnd();
    */
    
    /*
    glDisable(GL_BLEND);
    glEnable(GL_COLOR_LOGIC_OP);
    glLogicOp(GL_AND);
    for(int x = 0; x < 256; x++) {
        glColor4f(0.0, 0.0, 0.0, 1.0);
        glBegin(GL_QUADS);
        glVertex2f(x, 0);
        glVertex2f(x+0.001, 0);
        glVertex2f(x+0.001, 240);
        glVertex2f(x, 240);
        glEnd();
        
        glColor4f(0.0, 0.0, 0.0, 1.0);
        glBegin(GL_QUADS);
        glVertex2f(x+0.999, 0);
        glVertex2f(x+1, 0);
        glVertex2f(x+1, 240);
        glVertex2f(x+0.999, 240);
        glEnd();
        
        glColor4f(1.0, 0.0, 0.0, 1.0);
        glBegin(GL_QUADS);
        glVertex2f(x+0.001, 0);
        glVertex2f(x+0.334, 0);
        glVertex2f(x+0.334, 240);
        glVertex2f(x+0.001, 240);
        glEnd();
        
        glColor4f(0.0, 1.0, 0.0, 1.0);
        glBegin(GL_QUADS);
        glVertex2f(x+0.334, 0);
        glVertex2f(x+0.667, 0);
        glVertex2f(x+0.667, 240);
        glVertex2f(x+0.334, 240);
        glEnd();
        
        glColor4f(0.0, 0.0, 1.0, 1.0);
        glBegin(GL_QUADS);
        glVertex2f(x+0.667, 0);
        glVertex2f(x+0.999, 0);
        glVertex2f(x+0.999, 240);
        glVertex2f(x+0.667, 240);
        glEnd();
    }
    */
    
    glFlush();
    [[self openGLContext] flushBuffer];
}

@end

//
//  GameView.m
//  Legal Emulator
//
//  Created by Dan Knapp on 4/30/11.
//  Copyright 2011 Dan Knapp. All rights reserved.
//

#import "GameView.h"


@implementation GameView

- (void) awakeFromNib {
    initialized = NO;
}


- (BOOL) acceptsFirstResponder {
    return YES;
}


- (void) keyDown: (NSEvent *) event {
    switch([event keyCode]) {
    case 0: // a
        x--;
        [self setNeedsDisplay: YES];
        break;
    case 1: // s
        y++;
        [self setNeedsDisplay: YES];
        break;
    case 2: // d
        x++;
        [self setNeedsDisplay: YES];
        break;
    case 13: // w
        y--;
        [self setNeedsDisplay: YES];
        break;
    }
}


- (void) drawRect: (NSRect) dirtyRectangle {
    if(!initialized) {
        NSOpenGLContext *context = [self openGLContext];
        long swapInterval = 1;
        [context setValues: (const GLint *) &swapInterval
                 forParameter: NSOpenGLCPSwapInterval];
        
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
        
        x = 128;
        y = 120;
        initialized = YES;
    }
    
    NSLog(@"Drawing %i, %i\n", x, y);
    
    glClearColor(0.0f, 0.0f, 0.0f, 1.0f);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT);
    
    glFlush();
    [[self openGLContext] flushBuffer];
}

@end

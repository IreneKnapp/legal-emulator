//
//  GameView.h
//  Legal Emulator
//
//  Created by Dan Knapp on 4/30/11.
//  Copyright 2011 Dan Knapp. All rights reserved.
//

#import <Cocoa/Cocoa.h>


@interface GameView : NSOpenGLView {
    BOOL initialized;
    uint32_t nTextures;
    GLuint *textures;
    uint32_t currentTexture;
    GLuint temporaryTexture;
    GLuint shader;
    GLuint program;
    void *game;
}

- (void) awakeFromNib;
- (void) prepareGameFromFilename: (NSString *) filename;
- (BOOL) acceptsFirstResponder;
- (void) keyDown: (NSEvent *) event;
- (void) initOpenGL;
- (void) recomputeConvolutionKernel;
- (void) reshape;
- (void) initTextures;
- (void) drawRect: (NSRect) dirtyRectangle;
@end

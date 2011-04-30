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
    uint8_t x;
    uint8_t y;
}

- (void) awakeFromNib;
- (BOOL) acceptsFirstResponder;
- (void) keyDown: (NSEvent *) event;
- (void) drawRect: (NSRect) dirtyRectangle;
@end

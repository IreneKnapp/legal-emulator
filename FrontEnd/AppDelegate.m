//
//  AppDelegate.m
//  Legal Emulator
//
//  Created by Dan Knapp on 4/21/11.
//  Copyright 2011 Dan Knapp. All rights reserved.
//

#import "AppDelegate.h"
#import "Emulator.h"
#import "GameView.h"

@implementation AppDelegate

- (void) applicationDidFinishLaunching: (NSNotification *) notification {
    int result = emulator_init();
    if(result != 1) {
        NSLog(@"Internal failure to initialize emulation core.");
        exit(1);
    }
    
    [gameWindow setContentMinSize: NSMakeSize(256.0, 240.0)];
}


- (NSSize) windowWillResize: (NSWindow *) window
                     toSize: (NSSize) proposedFrameSize
{    
    if([window isEqual: gameWindow]) {
        NSSize proposedContentSize
            = [window contentRectForFrameRect:
                        NSMakeRect(0.0,
                                   0.0,
                                   proposedFrameSize.width,
                                   proposedFrameSize.height)].size;
        
        double enforcedAspect = 256.0 / 240.0;
        NSSize intermediateContentSize
            = NSMakeSize(proposedContentSize.height * enforcedAspect,
                         proposedContentSize.height);
        
        double intermediateScale = intermediateContentSize.width / 256.0;
        double enforcedScale = round(intermediateScale * 2.0) / 2.0;
        double scaleRatio = enforcedScale / intermediateScale;
        NSSize enforcedContentSize
            = NSMakeSize(intermediateContentSize.width * scaleRatio,
                         intermediateContentSize.height * scaleRatio);
        
        NSSize enforcedFrameSize
            = [window frameRectForContentRect:
                        NSMakeRect(0.0,
                                   0.0,
                                   enforcedContentSize.width,
                                   enforcedContentSize.height)].size;
        return enforcedFrameSize;
    } else {
        return proposedFrameSize;
    }
}


- (IBAction) addGames: (id) sender {
}


- (IBAction) removeGames: (id) sender {
}


- (IBAction) playGame: (id) sender {
    [gameWindow makeKeyAndOrderFront: self];
    [gameView setNeedsDisplay: YES];
}

@end

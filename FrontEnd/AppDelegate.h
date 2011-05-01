//
//  Legal_EmulatorAppDelegate.h
//  Legal Emulator
//
//  Created by Dan Knapp on 4/21/11.
//  Copyright 2011 Dan Knapp. All rights reserved.
//

#import <Cocoa/Cocoa.h>

@class GameView;
@interface AppDelegate : NSObject <NSApplicationDelegate> {
    IBOutlet NSWindow *gameListWindow;
    IBOutlet NSWindow *gameWindow;
    IBOutlet GameView *gameView;
}

- (void) applicationDidFinishLaunching: (NSNotification *) notification;
- (NSSize) windowWillResize: (NSWindow *) window
                     toSize: (NSSize) proposedFrameSize;
- (IBAction) addGames: (id) sender;
- (IBAction) removeGames: (id) sender;
- (IBAction) playGame: (id) sender;
@end

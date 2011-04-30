//
//  AppDelegate.m
//  Legal Emulator
//
//  Created by Dan Knapp on 4/21/11.
//  Copyright 2011 Dan Knapp. All rights reserved.
//

#import "AppDelegate.h"

@implementation AppDelegate

- (void) applicationDidFinishLaunching: (NSNotification *) notification {
}


- (IBAction) addGames: (id) sender {
}


- (IBAction) removeGames: (id) sender {
}


- (IBAction) playGame: (id) sender {
    [gameWindow makeKeyAndOrderFront: self];
    [openGLView setNeedsDisplay: YES];
}

@end

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

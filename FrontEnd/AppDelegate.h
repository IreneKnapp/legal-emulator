//
//  Legal_EmulatorAppDelegate.h
//  Legal Emulator
//
//  Created by Dan Knapp on 4/21/11.
//  Copyright 2011 Dan Knapp. All rights reserved.
//

#import <Cocoa/Cocoa.h>

@interface AppDelegate : NSObject <NSApplicationDelegate> {
    NSWindow *window;
}
@property (assign) IBOutlet NSWindow *window;

- (IBAction) addGames: (id) sender;
- (IBAction) removeGames: (id) sender;
@end

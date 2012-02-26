//
//  AppDelegate.h
//  logorrhea
//
//  Created by Ingrid Funie on 25/02/2012.
//  Copyright (c) 2012 ICL. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "SocketRocket/SRWebSocket.h"

#define BigDelegate \
  ((AppDelegate*) [UIApplication sharedApplication].delegate)

@interface AppDelegate : UIResponder <UIApplicationDelegate, SRWebSocketDelegate>
{
     IBOutlet NSString *username;
     SRWebSocket* myWS;
     bool loggedIn; 
}
@property (strong, nonatomic) UIWindow *window;

- (bool) getStatus;
- (NSString*) getUsername;
- (void) doConnect:(NSString *)username;
- (NSMutableDictionary *) makeRequest:(NSString *)event withData:(NSMutableDictionary*)data;
- (void) doListChannels;
- (void) connect:(NSString* )username;

@end

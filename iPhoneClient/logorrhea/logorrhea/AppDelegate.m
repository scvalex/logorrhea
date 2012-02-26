//
//  AppDelegate.m
//  logorrhea
//
//  Created by Ingrid Funie on 25/02/2012.
//  Copyright (c) 2012 ICL. All rights reserved.
//

#import "AppDelegate.h"
#import "SBJson.h"

@implementation AppDelegate

@synthesize window = _window;

- (BOOL)application:(UIApplication *)application didFinishLaunchingWithOptions:(NSDictionary *)launchOptions
{
    // Override point for customization after application launch.
    return YES;
}

- (void) webSocketDidOpen:(SRWebSocket *)webSocket
{
    NSLog(@"Socket-ul e gata");
    [self doConnect:username];
}

- (void) webSocket:(SRWebSocket *)webSocket didFailWithError:(NSError *)error
{
    NSLog(@"A murit aici %@", error);
}

- (void) webSocket:(SRWebSocket *)webSocket didReceiveMessage:(NSString *)message
{
    NSMutableDictionary* resp = [[[SBJsonParser alloc] init] objectWithString:message];
    
    if (resp == nil)
    {
        NSLog(@"Nu am putut parsa mesajul: %@", message);
    } else {
        NSString* event = [resp valueForKey:@"event"];
        if (event != nil && [event compare:@"connect.ok"] == 0) {
            loggedIn = true;
            NSLog(@"M-am conectat cu bine");
            NSLog(@"Am putut parsa mesajul: %@", event);
            [self doListChannels];
        } else if (event != nil && [event compare:@"list_channels.ok"] == 0) {
            NSDictionary* data = [resp valueForKey:@"data"];
            NSArray* channels = [data valueForKey:@"channels"];
            NSEnumerator *enumerator = [channels objectEnumerator];
            NSString* item;
            while (item = (NSString*)[enumerator nextObject]){
                NSLog(@"Am gasit canalul: %@", item);
            }
            //users
        } else {
            NSLog(@"Am primit un mesaj nerecunoscut: %@", message);
        }
    }
}

- (void) doListChannels {
    NSMutableDictionary* params = [[NSMutableDictionary alloc] init];
    
    NSString* request = [[[SBJsonWriter alloc] init] stringWithObject:[self makeRequest:@"list_channels" withData:params]];
    NSLog(@"voi lista canalele %@", request);
    
    [myWS send:request];
}

- (void) doConnect:(NSString *)user
{
    NSMutableDictionary* params = [[NSMutableDictionary alloc] init];
    [params setValue:user forKey:(@"user")];
    
    NSString* request = [[[SBJsonWriter alloc] init] stringWithObject:[self makeRequest:@"connect" withData:params]];
    NSLog(@"voi face acest request %@", request);
    
    [myWS send:request];
}

- (NSMutableDictionary *) makeRequest:(NSString *)event withData:(NSMutableDictionary*)data
{
    NSMutableDictionary* req = [[NSMutableDictionary alloc] init];
    [req setValue:event forKey:(@"event")];
    [req setValue:data forKey:(@"data")];
    
    return req;
}

- (void) connect:(NSString* )user{
    myWS = [[SRWebSocket alloc] initWithURLRequest:[NSURLRequest requestWithURL:[NSURL URLWithString:@"ws://129.31.197.27:9999/echo"]]];
    
    myWS.delegate = self;
    
    username = user;
    
    [myWS open];
}

- (bool) getStatus
{
    return loggedIn;
}

- (NSString* ) getUsername
{
    return username;
}
							
- (void)applicationWillResignActive:(UIApplication *)application
{
    /*
     Sent when the application is about to move from active to inactive state. This can occur for certain types of temporary interruptions (such as an incoming phone call or SMS message) or when the user quits the application and it begins the transition to the background state.
     Use this method to pause ongoing tasks, disable timers, and throttle down OpenGL ES frame rates. Games should use this method to pause the game.
     */
}

- (void)applicationDidEnterBackground:(UIApplication *)application
{
    /*
     Use this method to release shared resources, save user data, invalidate timers, and store enough application state information to restore your application to its current state in case it is terminated later. 
     If your application supports background execution, this method is called instead of applicationWillTerminate: when the user quits.
     */
}

- (void)applicationWillEnterForeground:(UIApplication *)application
{
    /*
     Called as part of the transition from the background to the inactive state; here you can undo many of the changes made on entering the background.
     */
}

- (void)applicationDidBecomeActive:(UIApplication *)application
{
    /*
     Restart any tasks that were paused (or not yet started) while the application was inactive. If the application was previously in the background, optionally refresh the user interface.
     */
}

- (void)applicationWillTerminate:(UIApplication *)application
{
    /*
     Called when the application is about to terminate.
     Save data if appropriate.
     See also applicationDidEnterBackground:.
     */
}

@end

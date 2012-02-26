//
//  LoginView.h
//  logorrhea
//
//  Created by Ingrid Funie on 25/02/2012.
//  Copyright (c) 2012 ICL. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "SocketRocket/SRWebSocket.h"

@interface LoginView : UIViewController<SRWebSocketDelegate> {
    IBOutlet NSString *username;
    bool logged_in;
    SRWebSocket* myWS;
}

@property (weak, nonatomic) IBOutlet UITextField *myusername;
@property(nonatomic, assign) bool logged_in;

-(IBAction)textFieldDoneEditing:(id)sender;
-(IBAction)backgroundClick:(id)sender;
-(IBAction)login;

- (void) doConnect:(NSString *)username;
- (NSMutableDictionary *) makeRequest:(NSString *)event withData:(NSMutableDictionary*)data;
- (void) doListChannels;

@end

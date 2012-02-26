//
//  LoginView.h
//  logorrhea
//
//  Created by Ingrid Funie on 25/02/2012.
//  Copyright (c) 2012 ICL. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "AppDelegate.h"

@interface LoginView : UIViewController {
    IBOutlet NSString *username;
}

@property (weak, nonatomic) IBOutlet UITextField *myusername;

-(IBAction)textFieldDoneEditing:(id)sender;
-(IBAction)backgroundClick:(id)sender;
-(IBAction)login;

@end

//
//  LoginView.h
//  logorrhea
//
//  Created by Ingrid Funie on 25/02/2012.
//  Copyright (c) 2012 ICL. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface LoginView : UIViewController {
    IBOutlet NSString *username;
    bool logged_in;
}

@property (weak, nonatomic) IBOutlet UITextField *myusername;
@property(nonatomic, assign) bool logged_in;

-(IBAction)textFieldDoneEditing:(id)sender;
-(IBAction)backgroundClick:(id)sender;
-(IBAction)login;

@end

//
//  LoginView.m
//  logorrhea
//
//  Created by Ingrid Funie on 25/02/2012.
//  Copyright (c) 2012 ICL. All rights reserved.
//

#import "LoginView.h"
#import "SBJson.h"
#import "AppDelegate.h"

@implementation LoginView

//@synthesize logged_in;
@synthesize myusername;
//@synthesize username;

- (id)initWithNibName:(NSString *)nibNameOrNil bundle:(NSBundle *)nibBundleOrNil
{
    self = [super initWithNibName:nibNameOrNil bundle:nibBundleOrNil];
    if (self) {
        // Custom initialization
    }
    return self;
}

- (IBAction)textFieldDoneEditing:(id)sender {
	
	[sender resignFirstResponder];
	
}

- (IBAction)backgroundClick:(id)sender {
    
	[myusername resignFirstResponder];
    
}

- (IBAction)login {
    username = myusername.text;
    
    [BigDelegate connect:username];
    
     NSLog(@"am deschis, cica");
    
    [self dismissModalViewControllerAnimated:(TRUE)];
}


- (void)didReceiveMemoryWarning
{
    // Releases the view if it doesn't have a superview.
    [super didReceiveMemoryWarning];
    
    // Release any cached data, images, etc that aren't in use.
}

#pragma mark - View lifecycle

- (void)viewDidLoad
{
    [super viewDidLoad];
    // Do any additional setup after loading the view from its nib.
    
    NSUserDefaults *name = [NSUserDefaults standardUserDefaults];
    myusername.text = [name stringForKey:@"textFieldKey"];
}

- (void) viewDidAppear:(BOOL)animated
{
    [super viewDidAppear:(TRUE)];
    
    if ([BigDelegate getStatus]) {
        UIAlertView *alert = [[UIAlertView alloc] initWithTitle:@"Error" message:@"You are already connected" delegate:nil cancelButtonTitle:@"Dismiss" otherButtonTitles:nil];
        [alert show];
        [self dismissModalViewControllerAnimated:(TRUE)];
    }
}

- (void)viewDidUnload
{
    [self setMyusername:nil];
    [super viewDidUnload];
    // Release any retained subviews of the main view.
    // e.g. self.myOutlet = nil;
}

- (BOOL)shouldAutorotateToInterfaceOrientation:(UIInterfaceOrientation)interfaceOrientation
{
    // Return YES for supported orientations
    return (interfaceOrientation == UIInterfaceOrientationPortrait);
}

@end

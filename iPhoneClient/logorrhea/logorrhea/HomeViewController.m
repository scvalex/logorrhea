//
//  FirstViewController.m
//  logorrhea
//
//  Created by Ingrid Funie on 25/02/2012.
//  Copyright (c) 2012 ICL. All rights reserved.
//

#import "HomeViewController.h"

@implementation HomeViewController

-(IBAction)showActionSheet {
    UIActionSheet *actionSheet = [[UIActionSheet alloc] initWithTitle:@"Let the story begin..." delegate:(id)self cancelButtonTitle:@"Cancel" destructiveButtonTitle:@"Continue" otherButtonTitles:@"Login",@"Register", @"Logout", nil];
    [actionSheet showInView:self.view];
}

- (void)actionSheet:(UIAlertView *)actionSheet clickedButtonAtIndex:(NSInteger)buttonIndex {
    
    // the user clicked one of the buttons
    switch (buttonIndex){
        case 0:
            // handle a cancel event
            break;
            
        case 1:
            //handle a login
            //[(id)self openLoginView];
            break;
            
        case 2:
            //handle a register
           // [(id)self openRegisterView];
            break;
            
        case 3:
            //[(id)self Logout];
            break;
            
        default:
            //do whatever
            break;
    }
    
}


- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Release any cached data, images, etc that aren't in use.
}

#pragma mark - View lifecycle

- (void)viewDidLoad
{
    [super viewDidLoad];
	// Do any additional setup after loading the view, typically from a nib.
}

- (void)viewDidUnload
{
    [super viewDidUnload];
    // Release any retained subviews of the main view.
    // e.g. self.myOutlet = nil;
}

- (void)viewWillAppear:(BOOL)animated
{
    [super viewWillAppear:animated];
}

- (void)viewDidAppear:(BOOL)animated
{
    [super viewDidAppear:animated];
}

- (void)viewWillDisappear:(BOOL)animated
{
	[super viewWillDisappear:animated];
}

- (void)viewDidDisappear:(BOOL)animated
{
	[super viewDidDisappear:animated];
}

- (BOOL)shouldAutorotateToInterfaceOrientation:(UIInterfaceOrientation)interfaceOrientation
{
    // Return YES for supported orientations
    if ([[UIDevice currentDevice] userInterfaceIdiom] == UIUserInterfaceIdiomPhone) {
        return (interfaceOrientation != UIInterfaceOrientationPortraitUpsideDown);
    } else {
        return YES;
    }
}

@end

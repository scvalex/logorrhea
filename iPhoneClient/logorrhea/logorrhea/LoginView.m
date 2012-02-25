//
//  LoginView.m
//  logorrhea
//
//  Created by Ingrid Funie on 25/02/2012.
//  Copyright (c) 2012 ICL. All rights reserved.
//

#import "LoginView.h"

@implementation LoginView
@synthesize logged_in;
@synthesize myusername;

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

- (NSString *)urlEncodeValue:(NSString *)str
{
    NSString *result = (__bridge NSString *) CFURLCreateStringByAddingPercentEscapes(kCFAllocatorDefault, (__bridge CFStringRef)str, NULL, CFSTR(":/?#[]@!$&’()*+,;=”"), kCFStringEncodingUTF8);
    
    return result;
}

- (IBAction)login {
    
    //check the crashing problem when connected to a wi-fi but without password.
   /* 
    NSURL *url = [NSURL URLWithString:@"http://www.doc.ic.ac.uk/project/2010/271/g1027114/users/login"];
    NSMutableURLRequest *request = [NSMutableURLRequest requestWithURL:url];
    [request setTimeoutInterval:5];
    [request setHTTPMethod:@"POST"];
    [request setValue:myusername.text forHTTPHeaderField:@"username"];
    [request setValue:mypassword.text forHTTPHeaderField:@"password"];
    
    NSString *text = [NSString stringWithFormat:@"username=%@&password=%@&Login=Login&format=json", [self urlEncodeValue:myusername.text], [self urlEncodeValue:mypassword.text]];
    
    NSData *requestBody =  [text dataUsingEncoding:NSUTF8StringEncoding];
    [request setHTTPBody:requestBody];
    
    username = NULL;
    
    NSURLResponse *response = NULL;
    NSError *requestError = NULL;
    NSData *responseData = [NSURLConnection sendSynchronousRequest:request returningResponse:&response error:&requestError];
    NSString *responseString = [[[NSString alloc] initWithData:responseData encoding:NSUTF8StringEncoding] autorelease];
    
    NSLog(@"%s", responseString);
    
    if (requestError)
	{
		UIAlertView *alert = [[UIAlertView alloc] initWithTitle:@"Error" message:@"Could not logon to server" delegate:nil cancelButtonTitle:@"Dismiss" otherButtonTitles:nil];
		[alert show];
		[alert release];
	}
	else if (response)
	{
        
        logged_in =  false;
        
        for (NSHTTPCookie *cookie in [[NSHTTPCookieStorage sharedHTTPCookieStorage] cookies]) 
            if ([[cookie name] isEqualToString:@"auth_tkt"]) {
                logged_in = true;
                username = myusername.text;
                [self dismissModalViewControllerAnimated:YES];
            }
        
        if (logged_in == false) {
            
            UIAlertView *alert = [[UIAlertView alloc] initWithTitle:@"Error" message:@"Wrong username/password " delegate:nil cancelButtonTitle:@"Dismiss" otherButtonTitles:nil];
            username = NULL;
            [alert show];
            [alert release];
            
        }
        
        
        
	}*/
    
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

//
//  LoginView.m
//  logorrhea
//
//  Created by Ingrid Funie on 25/02/2012.
//  Copyright (c) 2012 ICL. All rights reserved.
//

#import "LoginView.h"
#import "SBJson.h"

@implementation LoginView

@synthesize logged_in;
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

- (NSString *)urlEncodeValue:(NSString *)str
{
    NSString *result = (__bridge NSString *) CFURLCreateStringByAddingPercentEscapes(kCFAllocatorDefault, (__bridge CFStringRef)str, NULL, CFSTR(":/?#[]@!$&’()*+,;=”"), kCFStringEncodingUTF8);
    
    return result;
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

- (IBAction)login {
    
    username = myusername.text;
    
    myWS = [[SRWebSocket alloc] initWithURLRequest:[NSURLRequest requestWithURL:[NSURL URLWithString:@"ws://129.31.197.27:9999/echo"]]];
    myWS.delegate = self;
    
    [myWS open];
    
    NSLog(@"am deschis, cica");
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

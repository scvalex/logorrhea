//
//  DetailViewController.m
//  logorrhea
//
//  Created by Ingrid Funie on 26/02/2012.
//  Copyright (c) 2012 ICL. All rights reserved.
//

#import "ChatViewController.h"
//#import "Chat.h"
#import "AppDelegate.h"


@implementation ChatViewController

@synthesize chat;


#pragma mark -
#pragma mark View lifecycle

// When the view loads, define our data
- (void)viewDidLoad
{
    [super viewDidLoad];
    
    // Define our test data
    chat = [[NSMutableArray alloc] init];
}

- (void)viewDidAppear:(BOOL)animated
{
    chat = [BigDelegate getMessages];
}

// Return number of sections in table (always 1 for this demo!)
- (NSInteger)numberOfSectionsInTableView:(UITableView *)tableView
{
    return 1;
}

// Return the amount of items in our table (the total items in our array above)
- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section
{
    return [chat count];
}

- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath {
    
	// A cell identifier which matches our identifier in IB
    static NSString *CellIdentifier = @"CellIdentifier";
    
    // Create or reuse a cell
    UITableViewCell *cell = [tableView dequeueReusableCellWithIdentifier:CellIdentifier];
    if (cell == nil) {
        cell = [[UITableViewCell alloc] initWithStyle:UITableViewCellStyleDefault reuseIdentifier:CellIdentifier];
    }
    
    // Get the cell label using its tag and set it
    UILabel *cellLabel = (UILabel *)[cell viewWithTag:1];
    [cellLabel setText:[[chat objectAtIndex:indexPath.row] valueForKey:@"message"]];
    
    return cell;
}

#pragma mark -
#pragma mark Section header titles

/*
 HIG note: In this case, since the content of each section is obvious, there's probably no need to provide a title, but the code is useful for illustration.
 */
- (NSString *)tableView:(UITableView *)tableView titleForHeaderInSection:(NSInteger)section {
    
    NSString *title = nil;
    switch (section) {
        case 0:
            title = NSLocalizedString(@"Date", @"Date section title");
            break;
        case 1:
            title = NSLocalizedString(@"Genre", @"Genre section title");
            break;
        case 2:
            title = NSLocalizedString(@"Main Characters", @"Main Characters section title");
            break;
        default:
            break;
    }
    return title;
}


@end

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
    
    NSLog(@"sunt aici");
    
    // Define our test data
    chat = [[NSMutableArray alloc] init];
}

- (void)viewDidAppear:(BOOL)animated
{
    chat = [BigDelegate getMessages];
    NSEnumerator *enums = [chat objectEnumerator];
    NSString* disc;
    NSLog(@"Chat-ul are ");
    
    while(disc = (NSString*)[enums nextObject]){
        NSLog(@" Asta: %@ ", [disc valueForKey:@"message"]);
    }
    
    [self.tableView reloadData];
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

@end

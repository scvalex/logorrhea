//
//  TableViewController.m
//  logorrhea
//
//  Created by Ingrid Funie on 26/02/2012.
//  Copyright (c) 2012 ICL. All rights reserved.
//

#import "TableViewController.h"
#import "ItemViewController.h"
#import "DetailViewController.h"
#import "AppDelegate.h"

@implementation TableViewController

// When the view loads, define our data
- (void)viewDidLoad
{
    [super viewDidLoad];
    
    // Define our test data
   /* myData = [NSMutableArray arrayWithObjects:
              @"Chasing Amy",
              @"Mallrats",
              @"Dogma",
              @"Clerks",
              @"Jay &amp; Silent Bob Strike Back",
              @"Red State",
              @"Cop Out",
              @"Jersey Girl",
              nil];*/
    myData = [[NSMutableArray alloc] init ];
    
}

- (void)viewDidAppear:(BOOL)animated
{
    myData = [BigDelegate getChannels];
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
    return [myData count];
}

// Return a cell for the table
- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath
{
    // A cell identifier which matches our identifier in IB
    static NSString *CellIdentifier = @"CellIdentifier";
    
    // Create or reuse a cell
    UITableViewCell *cell = [tableView dequeueReusableCellWithIdentifier:CellIdentifier];
    if (cell == nil) {
        cell = [[UITableViewCell alloc] initWithStyle:UITableViewCellStyleDefault reuseIdentifier:CellIdentifier];
    }
    
    // Get the cell label using its tag and set it
    UILabel *cellLabel = (UILabel *)[cell viewWithTag:1];
    [cellLabel setText:[myData objectAtIndex:indexPath.row]];
    
    return cell;
}

- (void)tableView:(UITableView *)tableView didSelectRowAtIndexPath:(NSIndexPath *)indexPath
{
    
    // A cell identifier which matches our identifier in IB
    static NSString *CellIdentifier = @"CellIdentifier";
    
    // Create or reuse a cell
    UITableViewCell *cell = [tableView dequeueReusableCellWithIdentifier:CellIdentifier];
    if (cell == nil) {
        cell = [[UITableViewCell alloc] initWithStyle:UITableViewCellStyleDefault reuseIdentifier:CellIdentifier];
    }

    
    UILabel *cellLabel = (UILabel *)[cell viewWithTag:1];
    [cellLabel setText:[myData objectAtIndex:indexPath.row]];
    
    [BigDelegate doListConversations:cellLabel.text];
}

// Do some customisation of our new view when a table item has been selected
- (void)prepareForSegue:(UIStoryboardSegue *)segue sender:(id)sender
{
    // Make sure we're referring to the correct segue
    if ([[segue identifier] isEqualToString:@"ShowSelectedMovie"]) {
        
        // Get reference to the destination view controller
        ItemViewController *vc = [segue destinationViewController];
        
        // get the selected index
        NSInteger selectedIndex = [[self.tableView indexPathForSelectedRow] row];
        
        [vc setSelectedItem:[NSString stringWithFormat:@"%@", [myData objectAtIndex:selectedIndex]]];
        [vc setSelectedIndex:selectedIndex];
        
        
        DetailViewController *detailViewController = [segue destinationViewController];
        detailViewController.conversations = [myData objectAtIndex:selectedIndex];
    }
}

@end
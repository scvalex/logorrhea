//
//  ItemViewController.m
//  logorrhea
//
//  Created by Ingrid Funie on 26/02/2012.
//  Copyright (c) 2012 ICL. All rights reserved.
//

#import "ItemViewController.h"

@implementation ItemViewController

@synthesize selectedIndex, selectedItem;

- (void)viewDidLoad
{
    [super viewDidLoad];
    
    [outputLabel setText:selectedItem];
}

@end
#import "CommandScroll.h"
#import "LispText.h"
#import "Coordinator.h"


@implementation CommandScroll

// This is essentially from Subprocess.app

- initFrame:(const NXRect *)frameRect
{
    [super initFrame:frameRect];
    [self setVertScrollerRequired: YES];
    [self setBackgroundGray: NX_WHITE];
    [self setBorderType:NX_LINE];
    return self;
}

- awake
{
    NXRect textRect;

    textRect.origin.x = textRect.origin.y = 0.0;
    [self getContentSize: &(textRect.size)];
    textRect.size.width = 0.0;
    [docView setMinSize:&(textRect.size)];
    [self getContentSize: &(textRect.size)];
    textRect.size.height = 1000000;
    textRect.size.width = 100000;
    [docView setMaxSize:&(textRect.size)];
    [super setDocView:docView];
    return self;
}

- setDocView:anObject
{
    [super setDocView:anObject];
    docView = anObject;
    [docView setDelegate:self];
    return self;
}

- docView
{
    return docView;
}


@end

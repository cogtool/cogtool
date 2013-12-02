#import "MyPanel.h"

@implementation MyPanel

// Override keyDown and commandKey to respond only to normal keys and
// NOT to command keys

- keyDown:(NXEvent *)theEvent
{
    [super commandKey:theEvent]; // simulate a command key
    return self;
}

- (BOOL)commandKey:(NXEvent *)theEvent
{
    return NO;
}

@end

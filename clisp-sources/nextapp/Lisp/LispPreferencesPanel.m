#import "LispPreferencesPanel.h"

@implementation LispPreferencesPanel

// This is only to forward the changeFont: message to the coordinator
- changeFont:sender
{
    return [coordinator changeFont:sender];
}


@end

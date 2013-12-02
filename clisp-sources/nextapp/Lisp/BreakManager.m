#import "BreakManager.h"
#import "Coordinator.h"

@implementation BreakManager

- abort:sender
{
    [coordinator userEntered:"Abort\n"];
    return self;
}

- activate:sender
{
    [window orderFront:self];
    return self;
}

- backtrace:sender
{
    [coordinator userEntered:"Backtrace\n"];
    return self;
}

- bottom:sender
{
    [coordinator userEntered:"Bottom\n"];
    return self;
}

- continue:sender
{
    [coordinator userEntered:"Continue\n"];
    return self;
}

- down:sender
{
    [coordinator userEntered:"Down\n"];
    return self;
}

- mode1:sender
{
    [coordinator userEntered:"Mode-1\n"];
    return self;
}

- mode2:sender
{
    [coordinator userEntered:"Mode-2\n"];
    return self;
}

- mode3:sender
{
    [coordinator userEntered:"Mode-3\n"];
    return self;
}

- mode4:sender
{
    [coordinator userEntered:"Mode-4\n"];
    return self;
}

- next:sender
{
    [coordinator userEntered:"Next\n"];
    return self;
}

- over:sender
{
    [coordinator userEntered:"Over\n"];
    return self;
}

- step:sender
{
    [coordinator userEntered:"Step\n"];
    return self;
}

- top:sender
{
    [coordinator userEntered:"Top\n"];
    return self;
}

- up:sender
{
    [coordinator userEntered:"Up\n"];
    return self;
}

@end

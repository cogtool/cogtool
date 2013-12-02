#import <appkit/appkit.h>

@interface BreakManager:Object
{
    id  coordinator;
    id  window;
}

- abort:sender;
- activate:sender;
- backtrace:sender;
- bottom:sender;
- continue:sender;
- down:sender;
- mode1:sender;
- mode2:sender;
- mode3:sender;
- mode4:sender;
- next:sender;
- over:sender;
- step:sender;
- top:sender;
- up:sender;

@end

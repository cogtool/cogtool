/*
        CommandScroll.m
        by Joe Freeman
        Subprocess Example, Release 2.0
        NeXT Computer, Inc.

        You may freely copy, distribute and reuse the code in this example.
        NeXT disclaims any warranty of any kind, expressed or implied, as to
        its fitness for any particular use.

        Abgeändert für CLISP von Michael Stoll, November 1994
*/

#import <appkit/appkit.h>

@interface CommandScroll:ScrollView
{
    id  docView;
    id  delegate;
}

- docView;

@end

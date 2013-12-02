text( 32, bold, 'KLM Model' ).
text( 'Guy Pyrzak' ).
text( 'Febuary 2006' ).

:- ['cogtool.klm.CASCADE.pl'].

    /* --------------------------------

    SCHEDULING PREDICATES
    
    -------------------------------- */

schedule_name( 1 ).

schedule( 1, Strategy ) :-
  get_all_vars( start, Strategy, Vars ),
  
  labeling( [], Vars ),
  fd_statistics.
  
schedule( 8, Strategy ) :-
  get_all_vars( start, Strategy, SList ),
  get_all_vars( duration, Strategy, DList),
  get_processes_for_resource( Strategy, wm, Processes ),
  get_all_durations(Processes, Dur),
  doSquared(Dur,SDur),
  sum(SDur, #=, DurSum),
  domain([DurSum], 0, 1000000),
  append(SList, [DurSum], Vars1),
%  append(DList, [DurSum], Vars),
%  append(Vars1, DList , Vars3),
%  append(Dur, [DurSum], Vars4),
  write('pre vars3'),nl,
  labeling( [ minimize(DurSum)], Vars1 ),
  write(' pre vars 1'), nl,
%    labeling( [minimize(DurSum)], Vars),
  write('pre vars'), nl,
%  labeling([minimize(DurSum)], Vars),
 % write('post vars'), nl,
%  labeling( [], SList ),
  write('done'),
  fd_statistics.  
  
schedule( 9, Strategy ) :-
  get_all_vars( start, Strategy, SList ),
  get_all_vars( duration, Strategy, DList),
  get_processes_for_resource( Strategy, wm, Processes ),
  get_all_durations(Processes, Dur),
  sum(Dur, #=, DurSum),
  %domain([DurSum], 0, 10000),
  append(SList, [DurSum], Vars1),
%  append(DList, [DurSum], Vars),
 % append(Vars1, DList , Vars3),
%  append(Dur, [DurSum], Vars4),
  write('pre vars3'),nl,
  labeling( [ minimize(DurSum)], Vars1),
  write(' pre vars 1'), nl,
%    labeling( [minimize(DurSum)], Vars),
  write('pre vars'), nl,
%  labeling([minimize(DurSum)], Vars),
 % write('post vars'), nl,
%  labeling( [], SList ),
  write('done'),
  fd_statistics.  
  

  
  doConstrain([],[]).
  doConstrain([H|T],[U|V]):-
     H#=U,
     doConstrain(T,V).
  
  
  doSquared([],[]).
  doSquared([H|T],[U|V]):-
     U#=H * H,
     doSquared(T,V).
    /* The idea behind the following schedule predicate was that DURATIONS could be labelled prior to STARTS.  In this way, memory usage could be approximately minimized using an algorithm that was greedy for memory.  However, it appears that the complexity of the search space is still high.
    
    If greedy scheduling could be used efficiently to find approximately minimal memory use schedules then it could possibly also be used to find approximately minimal payoff schedules.  I.e. use a scheduling algorithm that was greedy for payoff rather than greedy for time.  AH November 2005.
    
    */
    
schedule( 2, Strategy ) :-
  write( '+ Label Durations using ff and only then label starts.'),nl,
  get_all_vars( duration, Strategy, DL ),
  write(DL),nl,
  get_all_vars( start, Strategy, SL ),
  labeling( [ff], DL ),
  %write(DL),nl,
  labeling( [ffc], SL ),
  fd_statistics.

    /* The following scheduling predicate also suffers from a lack of efficiency.  Scheduling durations explcitly seems hard.  Not sure at present why it has such negative consequences for the size of the search space.
    */
    
schedule( 3, Strategy ) :-
  write( '+ Greedy minimize durations.'),nl,
  get_all_vars( duration, Strategy, DL ),
  write(DL),nl,
  get_all_vars( start, Strategy, SL ),
  write(SL),nl,
  append( DL, SL, VARS ),
  labeling( [ffc, bisect], VARS ),
  fd_statistics.


/* optimize for total duration */

schedule( 4, Strategy ) :-
  get_all_vars( start, Strategy, SL ),
  get_all_vars( duration, Strategy, DL ),
  % get the max for the domain
  clp_duration_range( MINEND, MAXEND ),
  % set the domain for end since it's a new variable
  domain([END], MINEND, MAXEND), 
  % set the domain for SL cause it wasn't happy otherwise
  domain(SL, MINEND, MAXEND),
  % a function i wrote to contrain end to being larger than the end of all the processes
  gp_after(SL, DL, END),
  % make it one list of start times
  append(SL, [END], VARS),
  % label stuff, aka assign start times
  labeling( [minimize(END)], VARS ),
  fd_statistics.  
  
  
schedule( 5, Strategy, wm, WMCap ) :-
  write('attempting to schedule with schedule 5'),nl,
  findall(SItem ,sResourceSize( Name, _, SItem) ,SLIST),  
  sResourceSize(wm, _, WMCap),
  WMCap #= 1,
   write('pre labeling BIG list is'), write(SLIST), nl,
   write('pre labeling is'), write(WMCap), nl,
   write('pre start times are:'), write(Vars), nl,
  match_check_gp(WMCap, SLIST),
  delete(SLIST, WMCap, RL2),
  domain([WMCap], 1, 3),
  domain(RL2, 1, 50),
  append(RL2, [WMCap], NEWLIST),
  write('resource list is:'), write(NEWLIST), nl,
  labeling( [minimize(WMCap)], NEWLIST ),
   write('post labeling list is'), write(WMCap), nl,
  write('resource list is:'), write(NEWLIST), nl,
  write('start times are:'), write(Vars), nl

  .
  schedule( 5, Strategy, Name, WMCap):-
     sResourceSize(Name, _, WMCap)
  .

schedule( 6, Strategy ) :-
  get_all_vars( start, Strategy, Vars ),
  
  get_all_vars( amount, Strategy, ReqList ),
  % get all the wm guys
  get_processes_for_resource( Strategy, wm, Processes ),
  % get all the Req for the WM
  get_all_requirements( Processes, Req ),
  % set values to the req for WM from the highest possible values
  labeling( [down], Req),
  
  
  %if you want to use more then 1 then you'll need to get the Req for that and 
  % append the lists together. I will try to make a small set of functions to make
  % creating such a list easier for ya if you'd like. But for today this is it.
  
  % assign the start times with respect to the already set Req
  labeling( [], Vars ),
  
  write('trying amount'),write(Others),nl,
  fd_statistics.
  
schedule( 7, Strategy ) :-
  get_all_vars( start, Strategy, Vars ),
  get_processes_for_resource( Strategy, wm, Processes ),
  get_all_durations(Processes, Durations),
  labeling( [down], Durations),
  write('done labeling durations'),nl,
  labeling( [], Vars ),
  fd_statistics.
  
  

  match_check_gp(Wm, List):-
  
     member(Wm, List),
     write('wm is already in the list'),nl
  
  .
  
  match_check_gp(Wm, List):-
  
   write('wm is not in the list'),nl
  .
get_StreachyCapacity( Name, StreachyCapacity).


gp_after([], [], _). 
gp_after([S|Ss], [D|Ds], E) :- E #>= S+D, gp_after(Ss, Ds, E). 


task_color( file, '#999' ).
task_color( edit, '#DDD' ).

%task_color( pattern, '#DD0' ).
%task_color( tonetask, '#0DD' ).
%task_color( tonetask, '#0D0' ).

trials(100).

/*
 The Following MUST be set properly.
*/
clp_start_range( 0, 50000 ).
clp_duration_range( 0, 50000 ).

task_color( pattern, '#999' ).

    /* -------------------------------
    TASK
    note that 'file' and 'edit' tasks are used just so that different names
    are available for the two 'moveclick' tasks.  The names are used to set a 
    a color with thet task_color features.
    
    klm_TaskT1_Bravo - Done.
    
    --------------------------------*/

irg( "

top _ _
 --> 
 cogtool_core - Done.


cogtool_core -                   Done
-->
klm_m_op	 - _	 - RESULT1,
		klm_k_op key_x	 - RESULT1	 - RESULT2,
		klm_m_op	 - RESULT2	 - RESULT3,
		klm_k_op key_x	 - RESULT3	 - RESULT4,
		klm_m_op	 - RESULT4	 - RESULT5,
		klm_k_op key_x	 - RESULT5	 - RESULT6,
		klm_k_op key_x	 - RESULT6	 - RESULT7,
		klm_k_op key_x	 - RESULT7	 - RESULT8,
		klm_k_op key_x	 - RESULT8	 - RESULT9,
		klm_k_op key_x	 - RESULT9	 - RESULT10,
		klm_m_op	 - RESULT10	 - RESULT11,
		klm_k_op key_x	 - RESULT11	 - RESULT12,
		klm_m_op	 - RESULT12	 - RESULT13,
		klm_k_op key_x	 - RESULT13	 - RESULT14,
		klm_k_op key_x	 - RESULT14	 - RESULT15,
		klm_k_op key_x	 - RESULT15	 - RESULT16,
		klm_k_op key_x	 - RESULT16	 - RESULT17,
		klm_k_op key_x	 - RESULT17	 - RESULT18,
		klm_m_op	 - RESULT18	 - RESULT19,
		klm_k_op key_x	 - RESULT19	 - RESULT20,
		klm_m_op	 - RESULT20	 - RESULT21,
		klm_k_op key_x	 - RESULT21 - Done.").


irg("

 actsimple_move_hand DEVICE - SOURCES                                    - OUTPUT
 -->
     parameter( duration actsimple move hand DEVICE D),
     process([   distribution=basic
                 inputs=SOURCES
                 label=move_hand
                 duration=D
                 resource=hand
                 output=OUTPUT   ]).
                 
 actsimple_move DEVICE from WIDGET_START to WIDGET_DEST  - SOURCES       - OUTPUT 
 -->
     parameter( duration actsimple move DEVICE from WIDGET_START to WIDGET_DEST D),
     process([   distribution=basic
                 inputs=SOURCES
                 label=move
                 duration=D
                 resource=DEVICE
                 output=OUTPUT   ]).
 
 actsimple_drag DEVICE from WIDGET_START to WIDGET_DEST  - SOURCES       - OUTPUT
 -->
     parameter( duration actsimple drag DEVICE from WIDGET_START to WIDGET_DEST D),
     process([   distribution=basic
                 inputs=SOURCES
                 label=drag
                 duration=D
                 resource=DEVICE
                 output=OUTPUT   ]).
 
 actsimple_click DEVICE with HAND_FINGER                 - SOURCES       - OUTPUT
 -->
     parameter( duration actsimple click DEVICE with HAND_FINGER D),
     process([   distribution=basic
                 inputs=SOURCES
                 label=click
                 duration=D
                 resource=HAND_FINGER
                 output=OUTPUT   ]).
 
 actsimple_tap with HAND_FINGER                          - SOURCES       - OUTPUT
 -->
     parameter( duration actsimple tap with HAND_FINGER D),
     process([   distribution=basic
                 inputs=SOURCES
                 label=tap
                 duration=D
                 resource=HAND_FINGER
                 output=OUTPUT   ]).
 
 actsimple_press_mouse with HAND_FINGER                  - SOURCES       - OUTPUT
 -->
     parameter( duration actsimple press_mouse with HAND_FINGER D),
     process([   distribution=basic
                 inputs=SOURCES
                 label=press_mouse
                 duration=D
                 resource=HAND_FINGER
                 output=OUTPUT   ]).
 
 
 actsimple_release_mouse with HAND_FINGER                - SOURCES       - OUTPUT
 -->
     parameter( duration actsimple release_mouse with HAND_FINGER D),
     process([   distribution=basic
                 inputs=SOURCES
                 duration=D
                 label=release_mouse
                 resource=HAND_FINGER
                 output=OUTPUT   ]).
                 
 actsimple_press_key KEYBOARD_KEY                        - SOURCES       - OUTPUT
 -->
     parameter( duration actsimple press_key KEYBOARD_KEY HAND_FINGER D),
     process([   distribution=basic
                 inputs=SOURCES
                 label=press_key
                 duration=D
                 resource=HAND_FINGER
                 output=OUTPUT   ]).
                 
                 
 actsimple_press_key KEYBOARD_KEY with HAND_FINGER       - SOURCES       - OUTPUT
 -->
     parameter( duration actsimple press_key KEYBOARD_KEY HAND_FINGER D),
     process([   distribution=basic
                 inputs=SOURCES
                 resource=HAND_FINGER
                 label=press_key
                 duration=D
                 resource=HAND_FINGER
                 output=OUTPUT   ]).
 
 actsimple_graffiti_gesture CHARACTER                    - SOURCES       - OUTPUT
 -->
     parameter( duration actsimple graffiti gesture CHARACTER D),
     process([   distribution=basic
                 inputs=SOURCES
                 label=graffiti_gesture
                 duration=D
                 resource=right
                 output=OUTPUT   ]).
 
 actsimple_speak MSG                                     - SOURCES       - OUTPUT
 -->
     parameter( duration actsimple speak MSG D),
     process([   distribution=basic
                 inputs=SOURCES
                 label=speak
                 duration=D
                 resource=voice
                 output=OUTPUT   ]).
 
 actsimple_listen EVENT                                  - SOURCES       - OUTPUT
 -->
     parameter( duration actsimple listen EVENT D),
     process([   distribution=basic
                 inputs=SOURCES
                 label=listen
                 duration=D
                 resource=audition
                 output=OUTPUT   ]).
 
 actsimple_lookat WIDGET                                 - SOURCES       - OUTPUT
 -->
     parameter( duration actsimple lookat WIDGET D),
     process([   distribution=basic
                 inputs=SOURCES
                 label=lookat
                 duration=D
                 resource=eyes
                 output=OUTPUT   ]).
 
 actsimple_think INFORMATION                             - SOURCES       - OUTPUT
 -->
     parameter( duration actsimple think INFORMATION D),
     process([   distribution=basic
                 inputs=SOURCES
                 label=think
                 duration=D
                 resource=cognition
                 output=OUTPUT   ]).
 
 actsimple_recall INFORMATION                            - SOURCES       - OUTPUT
 -->
     parameter( duration actsimple recall INFORMATION D),
     process([   distribution=basic
                 inputs=SOURCES
                 label=recall
                 duration=D
                 resource=cognition
                 output=OUTPUT   ]).
         
 actsimple_system_wait SYSEVENT                          - SOURCES       - OUTPUT
 -->
     parameter( duration actsimple system_wait SYSEVENT D),
     process([   distribution=basic
                 inputs=SOURCES
                 label=system_wait
                 duration=D
                 resource=system
                 output=OUTPUT   ]).
     
 actsimple_do_transition STATE1 to STATE2            - SOURCES       - OUTPUT
 -->
     parameter( duration actsimple do_transition STATE1 to STATE2 D),
     process([   distribution=basic
                 inputs=SOURCES
                 label=sys_transition
                 duration=D
                 resource=system
                 output=OUTPUT   ]).
 
 klm_k_op EXPERTISE          - SOURCES                           - OUTPUT
 -->
     parameter( duration klm_k_op EXPERTISE D),
     process([   distribution=basic
                 duration=D
                 inputs=SOURCES
                 label=key_press
                 resource=hands
                 output=OUTPUT   ]).
 
 klm_m_op                    - SOURCES                           - OUTPUT
 -->
     parameter( duration klm_m_op D),
     process([   distribution=basic
                 duration=D
                 inputs=SOURCES
                 label=mental
                 resource=cognition
                 output=OUTPUT   ]).
 
 klm_d_op                    - SOURCES                           - OUTPUT
 -->
     parameter(  duration klm_d_op D),
     process([   distribution=basic
                 duration=D
                 inputs=SOURCES
                 label=draw
                 resource=hands
                 output=OUTPUT   ]).
             
 klm_h_op                    - SOURCES                           - OUTPUT
 -->
     parameter( duration klm_h_op D),
     process([   distribution=basic
                 duration=D
                 inputs=SOURCES
                 label=home
                 resource=hands
                 output=OUTPUT   ]).
 
 klm_p_op TARGET             - SOURCES                           - OUTPUT
 -->
     parameter( duration klm_p_op TARGET D),
     process([   distribution=basic
                 duration=D
                 inputs=SOURCES
                 label=point
                 resource=hands
                 output=OUTPUT   ]).
                 
 klm_r_op SYSEVENT       - SOURCES                               - OUTPUT
 -->
     parameter( duration klm_r_op SYSEVENT D),
     process([   distribution=basic
                 duration=D
                 inputs=SOURCES
                 label=system_response
                 resource=system
                 output=OUTPUT   ]).
 
 
 
 
").







/*
PARAMETERS
*/
     parameter( duration, actsimple, move, hand, _, 570).
     parameter( duration, actsimple, move, _, from, _, to, _, 1030).
     parameter( duration, actsimple, drag, _, from, _, to, _, 1030).
     parameter( duration, actsimple, click, _, with, _, 1030).
     parameter( duration, actsimple, tap, with, _, 20).
     parameter( duration, actsimple, press_mouse, with, _, 20).
     parameter( duration, actsimple, release_mouse, with, _, 20).
     parameter( duration, actsimple, press_key, _, _, 150).
     parameter( duration, actsimple, graffiti, gesture, _, 580).
     parameter( duration, actsimple, speak, _, 20).
     parameter( duration, actsimple, listen, _, 20).
     parameter( duration, actsimple, lookat, _, 20).     
     parameter( duration, actsimple, think, _, 1350).
     parameter( duration, actsimple, recall, _, 20).
     parameter( duration, actsimple, system_wait, _, 20).    
     parameter( duration, actsimple, do_transition, _, to, _, 20).
     parameter( duration, klm_m_op, 1350).
     parameter( duration, klm_d_op, 50).
     parameter( duration, klm_h_op, 400).
     parameter( duration, klm_r_op, _, 1000).
     parameter( duration, klm_k_op, _, 200).


    /* -------------------------------
    RESOURCES
    --------------------------------*/
 
% altering working memory
% vary from 1 to 5

 
resources( [
 (cognition,1),
 (wm,10),
    (eyes, 1),   
 (left,1),
 (right,1),
 (hand,2),
 (hands,1),
 (system,1),
 (mouse,1)
 ]).


	/* process constraint
       Andrew Howes
       This should be a library file.
       These are simple processes that implement discrete stage.
	*/

process( FEATURES ) :-
 % write('trying to run process before failed'),nl,
  match_process_features( [ distribution=DIST, inputs=SRCS, resource=RES,
    start=S, duration=D, quality=Q, id=ID, sflag=SFLAG, output=output( ID, RES, SFLAG, D, range(E1,E2 ) ) ], process(FEATURES) ),
  write('match worked'), nl,
  process( DIST, SRCS, ID, RES, S, D, Q, output( ID, RES, SFLAG, D,  range(E1,E2))),
  !.
  
process( basic, SRCS, ID, RES, S, D, _, output( ID, RES,  SFLAG, D,range(E1,E2)) ) :-
  impose_cascade_constraints( SRCS, S , RES),
  write('constraint for '), write(ID), write(' on '), write(RES), write(' s='),write(S), write(', d='), write(D), write(', e1='), write(E1), write(', e2='), write(E2), nl, 
  % E1 to E2 is the output range of the 
  E1 #= S + D - 1,
  E2 #= S + D, 
  write(' E1='), write(E1), write(', E2='), write(E2), nl
  .
process( buffer(MIN,MAX), SRCS, ID, RES, S, D, _, output( ID, RES, SFLAG, D, range(E1,E2)) ) :-
  write( 'duration for '), write(ID), write(' is '), write(D),write(' on '), write(RES),
  write(' E1='), write(E1), write(', E2='), write(E2),  nl,
  impose_cascade_constraints( SRCS, S , RES),
  % E1 to E2 is the output range of the process
  E1 #>= S + 1,
  E2 #=< S + D,
  D #>= MIN, D #=< MAX, 
  write('set buffer constraint'), nl
  .
process( buffer_opt(MIN,MAX), SRCS, ID, RES, S, D, _, output( ID, RES,  SFLAG, D,range(E1,E2)) ) :-
  write( 'OPT duration for '), write(ID), write(' is '), write(D),write(' on '), write(RES),
  write(' E1='), write(E1), write(', E2='), write(E2),  nl,
  impose_opt_cascade_constraints( SRCS, S , D, RES,ID),
  %impose_cascade_constraints( SRCS, S , RES),
  % E1 to E2 is the output range of the process
  E1 #= S + D,
  E2 #= S + D,
  ((D #>= MIN) #\ (D#=0)), D #=< MAX, 
  write('set buffer constraint'), nl
  .  
 
 process( refresh_opt(MIN,MAX), SRCS, ID, RES, S, D, _, output( ID, RES,  SFLAG, D,range(E1,E2)) ) :-
  write( 'OPT duration for '), write(ID), write(' is '), write(D),write(' on '), write(RES),
  write(' E1='), write(E1), write(', E2='), write(E2),  nl,
  impose_opt_cascade_constraints( SRCS, S , D, MIN, RES,ID),
  % E1 to E2 is the output range of the process
  write('pre e1  '), write(D), nl,
 % ( D #>= MIN #/\ E1 #= S + D - 1) #\ (E1 #= S+D),
   E1 #= S + D - 1,
  write('pre e2  '), write(D), nl,
  E2 #= S + D ,
  write('pre D MIN  '), write(D), nl,
  ((D #>= MIN) #\ (D#=0)), 
  write('pre D MAX'),nl,
  D #=< MAX, 
  write('set buffer constraint'), nl
  .   
  
process( normal(MEAN,STD), SRCS, ID, RES, S, D, _, output( ID, RES,  SFLAG, D,range(E1,E2)) ) :-
  normal( MEAN, STD, D ),
  write( 'duration for '), write(ID), write(' is '), write(D),write(' on '), write(RES), nl,
  impose_cascade_constraints( SRCS, S, RES ),
  % E1 to E2 is the output range of the process
  E1 #= S + D - 1,
  E2 #= S + D.
  
process( concurrent, SRCS, ID, RES, S, D, _, output( ID, RES,  SFLAG, D,range(E1,E2)) ) :-
  write( 'duration for '), write(ID), write(' is '), write(D),write(' on '), write(RES), nl,
  impose_cascade_constraints( SRCS, S, RES ),

  % E1 to E2 is the output range of the process
  E1 #= S,
  E2 #= S + D/2.
  
 
process( fittslaw_obj(OBJ_FROM, OBJ_TO), SRCS, ID, RES, S, D, _, output(ID, RES, SFLAG, D, range(E1, E2)) ) :-
  % get x1 and y1 from the first object
  parameter( widget, info, OBJ_FROM, X1, Y1, _),
  % get x2 and y2 and the SIZE from the second object
  parameter( widget, info, OBJ_TO, X2, Y2, SIZE),
  % calc fitts
  fitts_move_time(X1, Y1, X2, Y2, SIZE, D),
  
  impose_cascade_constraints( SRCS, S, RES ),
  % E1 to E2 is the output range of the process
  E1 #= S + D - 1,
  E2 #= S + D.

 
process( fittslaw(DISTANCE, SIZE), SRCS, ID, RES, S, D, _, output(ID, RES, SFLAG, D, range(E1, E2)) ) :-
  fitts_move_time( DISTANCE, SIZE, D ),
  impose_cascade_constraints( SRCS, S, RES ),
  % E1 to E2 is the output range of the process
  E1 #= S + D - 1,
  E2 #= S + D.

fitts_move_time( Dist, Size, Time ) :-
  Time is integer( 100 * log( 0.5 + Dist / Size ) ).

fitts_move_time(X1, Y1, X2, Y2, Size, Time) :-
	calc_distance(X1, Y1, X2, Y2, Dist),
	fitts_move_time(Dist, Size, Time).

calc_distance(X1, Y1, X2, Y2, DIST) :-
	DIST is integer(sqrt( ((X2-X1) ** 2) + ((Y2-Y1) ** 2))).

/*
FITTS LAW
Tpos = Im log2 (D/ S + .5),
Tpos = Time to move to the given position
Im = 100[ 70~ 120] msec/ bit
D is the distance
S is the size both along the same axis
calculate the distance? 
what do they have for me? if they can just give me the D and the S that would be best.



here is where we might want to calculate the size based on the direction.
calc_size( X1, Y1, X2, Y2, HEIGHT, WIDTH, SIZE) :-
	% calc the slope
	SLOPE is ((Y1 - Y2)/(X1-X2)),
*/


/*
process( cmuwest(MEAN,STD,PEAKQ), SRCS, ID, RES, S, D, Q, output( ID, RES, range(E1,E2)) ) :-
  normal( MEAN, STD, D ),
  impose_cascade_constraints( SRCS, S ),
  % E1 to E2 is the output range of the process
  E1 #= S + D - 1,
  E2 #= S + D.
*/
process( actr(BASE,SRC,Noise_parameter), SRCS, ID, RES, S, D, _, output( ID, RES,  SFLAG, D, range(E1,E2)) ) :-
  actr_retrieval_time( BASE, SRC, Noise_parameter, D ),
  impose_cascade_constraints( SRCS, S ),
  % E1 to E2 is the output range of the process
  E1 #= S + D - 1,
  E2 #= S + D.



% is there a way to know the id of my parent?? Can i identify my parent?
% the parent does not know the child, but the child must know the parent?? do I know the parent? 
% to know I must label the ID
impose_opt_cascade_constraints( [], _ , _, _, _, ID).
  %call( (D#=0 #/\ CD#= 0)),D#>0 #/\ 
  
impose_opt_cascade_constraints( SOURCE, S, CD, MIN, RES,ID ) :-
  write('entering opt constraint'),
  SOURCE = output( PID, SRCRES, SFLAG, D, range(S1,S2) ),
  
  call( S1 #=< S ),
  call( S #=< S2 ),
  write(' pre or constraint '),  write( '--'),

  %call(  (D#=0 #/\ CD#=0) #\/ ( D#> 0 #/\ (CD#=>MIN #\/ CD #=0))  ),
  call( (D#=0 #/\ CD#=0) #\ (D#>0 #/\(CD#>=MIN #\/ CD#=0))),
  
  write(' imposing contstrains OPT for: ' - SOURCE ), nl,
  !.
impose_opt_cascade_constraints( [H|T], S , CD, MIN, RES,ID) :-
  impose_opt_cascade_constraints( H, S , CD, MIN, RES,ID),
  impose_opt_cascade_constraints( T, S , CD, MIN, RES,ID).
impose_opt_cascade_constraints( SOURCE, S , CD, MIN, RES,ID) :-
 	SOURCE = output( _, _,  SFLAG, D, range(S1,S2) ),
 	 call( S1 #=< S ),
  	 call( S #=< S2 ),
%  	 call( CD #= 0),
 	 write( '* Failed to impose OPT constraint: ' - SOURCE ),nl.




% original cascade constraints
impose_cascade_constraints( [], _ , _).
impose_cascade_constraints( SOURCE, S, RES ) :-
  SOURCE = output( _, SRCRES,   SFLAG, D, range(S1,S2) ),
  call( S1 #=< S ),
  call( S #=< S2 ), 
  write(' imposing contstrains for: ' - SOURCE ), nl,
  !.
impose_cascade_constraints( [H|T], S , RES) :-
  impose_cascade_constraints( H, S , RES),
  impose_cascade_constraints( T, S , RES).
  
impose_cascade_constraints( SOURCE, _ , RES) :-
 	SOURCE = output( _, _,  SFLAG, D, range(S1,S2) ),
 	 write( '* Failed to impose constraint: ' - SOURCE ),nl.

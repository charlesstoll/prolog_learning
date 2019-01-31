%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Section 1 : Game's Facts
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% These two numbers set how clever the AI is 
% (how many levels will it scan)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
win_deep(2).		% Attack rating
nolose_deep(2).		% Defence rating
random_factor(3).	% AI random factor. chance of success is 1/(X-1)

player_human(h).
player_ai(c).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% board_sign facts are used to determin the game sign
% of the guman and computer:
% h - Human, c - Computer, f - Empty.
% This rules are created dynamically at the setup 
% phase of the game.
%
% Example:
%  board_sign(h,x).
%  board_sign(c,o).
%  board_sign(f,f).
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Game board description
% ----------------------
% The game's board is represented by dynamic facts:
% game(Board).
% Board representation:	f - empty spot
%			h - human's spot
%			c - computer's spot
% Row = [lowers,...,highest], lowest var index number is 1   
% Each sublist represents 1 column.
% The board size if represented by board_size(N).
%
% Example:
% board_size(8).
% game([[f,f,f,f],
%    	[f,f,f,f],
%	[f,f,f,f],
%	[f,f,f,f]]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Section 2 : Game's Execution statement.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
main:- write_help,game_start.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% game_start is divided to 3 rules:
% 1. Setup game - create board, assign which player starts
% 2. Id human starts get input from human
% 3. If computer starts have the computer make his move
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
game_start:-setup, nl, fail.
game_start:-board_sign(h,x),game(Board), show_board(Board), get_input(Board),!. % Player starts, Cut prevents backtracing
game_start:-board_sign(h,o),game(Board),think(Board),!. % Computer starts, Cut prevents backtracing

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Section 3 : Game's Rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% go(X).
% go performs the player's move and call the computer.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% first check the move is legal (Column not full)
go(X):-  game(Pos),
	 not(get_y(X,Y)),	% continue if column is full, else fail
	 say(Pos, 'Move Illegal! You cant move to the specified column. Choose again...', yes).

go(X):-  get_y(X,Y),			% Get Y value for X
	 game(Current_Pos),		% Get current board position
	 member(Current_Pos, L, X),	% Get the X list - L		
	 member(L, f, Y),
	 !, % This cut is used to prevent prolog from backtracing into previous turns
	 player_human(HSign), 	 	% Get the sign of human player
       	 replace(L2, L, HSign, Y),	% Put human's sign at its place in the X column
       	 replace(Current_Pos2, Current_Pos, L2, X), % Update the board
       	 think(Current_Pos2). 		% Call AI with updated board

% think is the AI rule. the AI tries to do certain actions
% by their priority.
% 1 - Check if it has been defeated.
% 2 - Check if there's a move that will make him win
% 3,4 - Prevent player from winning. Block player if he's about to win.
% 5 - Make a move without allowing the player to make a winning move afterwards
% 6 - Make any move
%
% The cuts in each rule's end are used to prevent prolog from backtracing to 
% previous, already played, turns.
think(Pos) :-
  	player_human(H), victory(H, Pos), !,
  	say(Pos, 'Unbelievable! You are the winner.', no).
think(Pos) :-
  	try_to_win(Pos), !.
think(Pos) :-
  	retractall(last_good_move(_)), 
	try_nolose(Pos).
think(_)   :-
  	last_good_move(Pos2), !, % prevents backtracking that will cause the 
				 % computer to change a move he had
				 % already done and look for a new one.
  	say(Pos2, 'Its your move.', yes).
think(Pos) :-
  	player_ai(AI), player_human(H),
	move(AI, Pos, Pos2),
  	nolose(H, Pos2, 0), !, % Tries to advance toward winning
  	say(Pos2, 'Go on, you have some chance in this game.', yes).
think(Pos) :-
  	player_ai(AI),
  	move(AI, Pos, Pos2),
  	say(Pos2, 'Go on, you have some chance in this game.', yes).

try_nolose(Pos) :- player_ai(AI), 
		   move(AI, Pos, Pos2),
                   nolose_deep(Deep),
		   player_human(H), 
		   nolose(H, Pos2, Deep),
                   retractall(last_good_move(_)),assert(last_good_move(Pos2)),
		   random_factor(F), random_number(X,F), X == 1, !, fail. % Make AI's move more inetersting

try_to_win(Pos) :- player_ai(AI),
		   move(AI, Pos, Pos2),
                   is_it_win(Pos2).


is_it_win(Pos2) :-
  player_ai(AI),
  victory(AI, Pos2),
  say(Pos2, 'Sorry, you cannot win against the AI.', no).

is_it_win(Pos2) :-
  win_deep(Deep),
  player_human(H),
  win(H, Pos2, Deep),
  say(Pos2, 'Give up. Dont lose my time.', yes).


% AI scanning rules
% -----------------------------
% AI is trying to win
win(c, Pos, Deep) 	:- move(c, Pos, Pos2), win(h, Pos2, Deep).
win(h, Pos, _) 		:- victory(c, Pos). % AI Wins
win(h, Pos, 0) 		:- !, fail. % Cant go any deeper
win(h, Pos, Deep) 	:- Deep1 is Deep - 1, not(nowin(h, Pos, Deep1)). % find a move that will not cause X (Player) to win

nowin(h, Pos, _) 	:- not(move(Pos)). 
nowin(h, Pos, Deep) 	:- move(h, Pos, Pos2), nowin(c, Pos2, Deep).% Check if after this move X (Player) will be able to make a winning move
nowin(c, Pos, _) 	:- victory(h, Pos). % move caused X (Player) to win
nowin(c, Pos, Deep) 	:- not(win(c, Pos, Deep)).

%%%

lose(h, Pos, Deep) :- move(h, Pos, Pos2), lose(c, Pos2, Deep).
lose(c, Pos, _)    :- victory(h, Pos). % Check if AI lost bcz of Player's move
lose(c, Pos, 0)    :- !, fail.         % Cant go any deeper
lose(c, Pos, Deep) :- Deep1 is Deep - 1, not(nolose(c, Pos, Deep1)). % AI's move

% nolose(o, Pos, Deep) - finds a move that will not cause AI to loose.
nolose(c, Pos, Deep) :- move(c, Pos, Pos2), nolose(h, Pos2, Deep).
nolose(h, Pos, _)    :- victory(c, Pos).   % Check if this board will cause O (AI) to win
nolose(h, Pos, _)    :- not(move(Pos)).    % The last move belongs to O
nolose(h, Pos, Deep) :- not(lose(h, Pos, Deep)). % check that a move by Player wont cause AI to lose 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% say(Board_Position, Message, Continue).
% This is the computer's play result, it shows the
% board position and a nasty remark from the AI :)
% Continue signals if the game is over or not.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
show_board(Pos):-
	write_board(Pos).

say(Pos, Message, Continue) :-
  write(Message),
  nl,
  show_board(Pos),
  say_continue(Pos, Continue).
  
say_continue(Pos, yes) :-
  get_input(Pos).

say_continue(Pos, no) :-quit,!. % The cut is used in order to stop the program's running

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% member determines Element's position in the list
% member(List, Element, N).
% first var is marked as N=1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
member([Element|_],Element,1):-!. 
member([_|Tail],Element,N):-
		M is (N-1),
		member(Tail,Element,M).

% same as member 1 but without the cut
% example of use: member2([1,2,3,4],X,N).
member2([Element|_],Element,1).
member2([_|Tail],Element,N):-
		member2(Tail,Element,N1),
		N is (N1+1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% replace(NewList, List, Element, N).
% replaces the N item of List (leftest=1) with Element 
% and results NewList
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
replace([Element|Tail],[_|Tail],Element,1).
replace([Head|TailNew],[Head|Tail],Element,N):-
			M is (N-1),
			replace(TailNew,Tail,Element,M).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% check_four(Who, Board).
%% Checks Board to see if player Who won using
%% one of the states that requires 4 colums:
%% Horizonal, Right\Left slash - -,/,\
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
horiz_checkup(Who, Col1, Col2, Col3, Col4):-
	member2(Col1, Who, N),
	member2(Col2, Who, N),
	member2(Col3, Who, N),
	member2(Col4, Who, N).

rightslash_checkup(Who, Col1, Col2, Col3, Col4):-
	member2(Col1, Who, N1),
	N2 is N1+1,
	N3 is N2+1,
	N4 is N3+1,
	member2(Col2, Who, N2),
	member2(Col3, Who, N3),
	member2(Col4, Who, N4).

leftslash_checkup(Who, Col1, Col2, Col3, Col4):-
	member2(Col1, Who, N4),
	N3 is N4-1,
	N2 is N3-1,
	N1 is N2-1,
	member2(Col2, Who, N3),
	member2(Col3, Who, N2),
	member2(Col4, Who, N1).

four_checkup(Who,Col1,Col2,Col3,Col4) :- horiz_checkup(Who,Col1,Col2,Col3,Col4),!. 
four_checkup(Who,Col1,Col2,Col3,Col4) :- leftslash_checkup(Who,Col1,Col2,Col3,Col4),!.
four_checkup(Who,Col1,Col2,Col3,Col4) :- rightslash_checkup(Who,Col1,Col2,Col3,Col4).

% collect the board's columns to facts
check_four_process(Who, Board, N1):-
	N2 is N1+1,
	N3 is N2+1,
	N4 is N3+1,
	member2(Board, Col1, N1),
	member2(Board, Col2, N2),
	member2(Board, Col3, N3),
	member2(Board, Col4, N4),
	four_checkup(Who,Col1,Col2,Col3,Col4),
	assert(game_won(Who)).

check_four(Who, Board):-
	board_size(BSize),
	Top is BSize-3,
	for(X,1,Top,1),
	check_four_process(Who,Board,X),
	fail.
check_four(Who, Board):-
	game_won(Who),
	retractall(game_won(Who)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% check_vert(Who, Board).
%% Checks Board to see if player Who won vertically.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
check_vert(Who,Board):-
	member2(Board, Col, N),
	sublist([Who,Who,Who,Who],Col).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% victory(Who, Board).
%% Checks Board to see if player Who won.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
victory( Who, Board ):-check_four(Who,Board).
victory( Who, Board ):-check_vert(Who,Board).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get_y(X)
% gets Y value for given X. Y is the lowest availble
% item in the X column (availble=marked with f)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_y(X,Y) :- game(Board),
		member(Board, XList, X), % Get the list for X
		find_first_free(XList,Y).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% find_first_free(List, N).
% Find the first free pos in List (marked by f) and 
% return its position to N
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
find_first_free([f|Tail],1):-!.  % the ! is meant so there will be only 1 result
find_first_free([Head|Tail],N):-find_first_free(Tail, M),
   			     		 N1 is M+1,
			     		 N is N1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% move puts P on a place on an availble place on the board
% (an availble place is a place marked with f)
% P - value (o\x) , Pos - original board , Pos2 - new board
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
move(P, Pos, Pos2) :-
  member2(Pos, L, X),		% gets column
  find_first_free(L,Y),
  replace(L2, L, P, Y),
  replace(Pos2, Pos, L2, X).

% checks that row isnt full (still have spots marked with f)
move(Pos) :-
  member2(Pos, L, _),
  member2(L,f,_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% write_board(Pos) - outputs the board Pos.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
write_item(Item):-
	board_sign(Item, Out),
	write(Out).

write_col(Col,N):-
	member(Col, Item, N),
	write(' '),write_item(Item).

write_board_process(Pos,N):-
	board_size(BSize),
	for(X,1,BSize,1),
	member(Pos, Col, X), % Get the X column
	write_col(Col, N),
	fail.

write_board(Pos):-
	board_size(BSize),
	BSize1 is BSize+1,
	for(X,1,BSize,1),
	X1 is BSize1-X,
	not(write_board_process(Pos,X1)),
	nl,
	fail.
write_board(Pos):-
	board_size(BSize),
	for(X,1,BSize,1),
	write(' '),write(X), % write the column's numbers
	fail.
write_board(Pos):-nl.	
		
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Section 4 : Interface Rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get_input(Pos) gets input gets the input from 
% the human player
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_input(Pos):-	
	nl,
	assert(current_position(Pos)), % save current position
	get_command(X),
	do(X).

% The cuts in the do() rules prevent prolog from backtracing back to the command prompt
do(makemove(X)):-makemove(X),!.
do(set(GameVar,Value)):-set(GameVar,Value),!.
do(show_gamevars):-show_gamevars,!.
do(help):-help,!.
do(show):-show,!.
do(quit):-quit,!.
do(reset):-game_start.

% makemove(X) makes the player's move
makemove(X):-
	string_atom(String, X),
	string_integer(String, N),
	current_position(Pos),
	retractall(game(_)),retractall(current_position(_)),
	assert(game(Pos)),
	go(N).

% help shows the help screen
help:-  write_help, show.

% show output the current board
show:-  current_position(Pos), nl,show_board(Pos),
	retractall(current_position(_)),
	get_input(Pos).

write_help:-
	writeln('FOUR IN A ROW - Prolog final project'),
	writeln('Made By Eran Kampf'),
	writeln(''),
	writeln('GAME DESCRIPTION:'),
	writeln('The game board is at size N*N which represent N columns on which each'),
	writeln('player can slide his sign. Each player, at his turn, puts his sign on'),
	writeln('one of the columns at it "slides" down the colums to the lowest place possible.'),
	writeln('In order to win the game a play must have 4 signs placed together horizonally,'),
	writeln('vertically, or a diagonal.'),
	writeln(''),
	writeln('GAME CONTROL:'),
	writeln('At the beginning of the game you will be ask to enter the board size.'),
	writeln('The game itself is controlled using simple English commands.'),
	writeln(''),
	writeln('- Type ''go'',''goto'',''move'',etc and the column number to make your move.'),
	writeln('  You can also just type a number. Example: ''go 2'', ''2'',''goto 2''.'),
	writeln('- Type ''help'' to read these instructions again.'),
	writeln('- Type ''quit'' in order to finish the game.'),
	writeln('- Type ''show'',''board'',''pos'' to review the current Board position'),
	writeln('- Type ''reset'' to reset the game.'),
	writeln('- Type ''set <game var> <value>'' to change the value of one of the game vars.'),
	writeln('  The game vars are : random_factor, win_deep, nolose_deep.'),
	writeln('  Example: ''set random_factor 3'' '),
	writeln('- Type ''gamevars'' in order to see the current values of the game vars'),
	writeln(''),
  	writeln('Hit any key to continue.'),get0(_).
		
% Sets the random_factor value
set(random_factor, Value):-
	atom_number(Value, IntValue),
	IntValue>2,   % factor of 2 means 1/1 chance to miss the move - computer wont move
	retractall(random_factor(_)),assert(random_factor(IntValue)),
	respond(['random_factor value set to ',IntValue]),
	show.

% Sets the win_deep value
set(win_deep, Value):-
	atom_number(Value, IntValue),
	retractall(win_deep(_)), assert(win_deep(IntValue)),!,
	respond(['win_deep value set to ',IntValue]),
	show.

% Sets the nolose_deep value
set(nolose_deep, Value):-
	atom_number(Value, IntValue),
	retractall(nolose_deep(_)),assert(nolose_deep(IntValue)),
	respond(['nolose_deep value set to ',IntValue]),
	show.

% Outputs the values of the game vars
show_gamevars:-
	random_factor(RandFac),
	win_deep(Win),
	nolose_deep(Nolose),
	respond(['win_deep :',Win]),
	respond(['nolose_deep :',Nolose]),
	respond(['random_factor :',RandFac]),show.

% quits game
quit:-
	nl,
	writeln('Quiting program.'),
	retractall(current_position(_)),
	retractall(game(_)).

%%%%%%%%%%%%%%%%%%%

get_command(C):-
	read_list(L),	% reads sentence and put in list.
  	command(X,L,[]),    % call the grammar for command
  	C =.. X,!.          % make the command list a structure
get_command(C):-
  respond(['I don''t understand, try again or type help']),get_command(C).
	
% The grammar doesn't have to be real English.  
% There are two types of commands , those with and without a 
% single argument.
% A special case is also made for the command makemove which can be 
% activated simply by typing a number.
command([Pred,Arg]) --> verb(Type,Pred),
			nounphrase(Type,Arg).   % command + argument
command([Pred,Arg1,Arg2]) --> 	
			verb(Type,Pred),
			nounphrase(Type,Arg1,Arg2).  % command + 2 arguments

command([Pred])     --> verb(misc,Pred).	% command only (no arguments)
command([makemove,Arg]) --> noun(make_move,Arg).% special case - makemove command

nounphrase(Type,Noun) --> noun(Type,Noun).
nounphrase(Type,Noun1,Noun2) --> noun(Type,Noun1,Noun2).

verb(make_move,makemove) --> go_verb.
verb(misc,V) --> misc_verb(V).
verb(set, V) --> set_verb(V).

% define the verbs that will cause execusion of the makemove
go_verb --> [go].
go_verb --> [go,to].
go_verb --> [move].
go_verb --> [move,to].
go_verb --> [g].

% other commands
misc_verb(help) --> [help].	% Show Help
misc_verb(show) --> [show].	% Show Board
misc_verb(show) --> [show,board].% Show Board
misc_verb(show) --> [board].	% Show Board
misc_verb(show) --> [pos].	% Show Board
misc_verb(reset) --> [reset].	% Reset game
misc_verb(reset) --> [startover].% Reset game
misc_verb(quit) --> [quit].	% Quit Game
misc_verb(quit) --> [exit].	% Quit Game
misc_verb(quit) --> [end].	% Quit Game
misc_verb(quit) --> [bye].	% Quit Game

misc_verb(show_gamevars) --> [gamevars]. % show the game vars
set_verb(set)	--> [set].
set_verb(set)	--> [config].

noun(make_move,N) --> [N].
noun(set, Var, Value) --> [Var,Value].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% read_list(L) - reads a normal sentence from the user and puts 
%% it into a list.  It is composed of two parts: 
%% 1. Reads a line of ASCII characters from the user, using the 
%%    built-in predicate get0/1, which reads a single ASCII character.
%%    The line should be terminated by one of the lastword(X) keys. 
%% 2. Part uses DCG to parse the list of characters into a list 
%%    of words, using another built-in predicate name/2, which  
%%    converts a list of ASCII characters into an atom. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
read_list(L) :-
  write('Enter Command: '),
  read_line(CL),
  wordlist(L,CL,[]), !. 

read_line(L) :-
  get0(C),
  buildlist(C,L).

buildlist(C,[]) :- lastword(C),!. % Cut is used to stop when reached to a terminal char
buildlist(C,[C|X]) :-
  get0(C2),
  buildlist(C2,X).
 
wordlist([X|Y]) --> word(X), whitespace, wordlist(Y).
wordlist([X]) --> whitespace, wordlist(X).
wordlist([X]) --> word(X).
wordlist([X]) --> word(X), whitespace.

word(W) --> charlist(X), {name(W,X)}.

charlist([X|Y]) --> chr(X), charlist(Y).
charlist([X]) --> chr(X).

chr(X) --> [X],{X>=48}.

whitespace --> whsp, whitespace.
whitespace --> whsp.

whsp --> [X], {X<48}.
lastword(10).   % end if new line entered
lastword(`.`).
lastword(`!`).
lastword(`?`).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Section 4 : Program Setup rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Reads an integer input
read_int(X):-read(X),integer(X),!. % Cut used to stop when an integer is read
read_int(X):-writeln('Error'),read_int(X).

% Read a yn input
yesno(`y`).
yesno(`n`).
read_yn(X):-get(X), yesno(X),!.   % Cut is used to stop when a yesno(X) char 
				  % is read
read_yn(X):-writeln('Error'), read_yn(X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% setup rule is the rule that setups the game.
% It is divided into 2 parts:
% 1. Create the board according to user input
% 2. Assign the player's signs where the player
%    who opens the game is X and the other is O
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
setup:- write('Enter Board Size: '),
	read_int(BSize),
	create_board(BSize,Board),
	nl, fail.

setup:-	retractall(board_sign(_,_)),
	write('Do you want to start? (y / n) - '), read_yn(Chr),
	set_players(Chr).

% set_players set the players signs according to who opens the game.
set_players(`y`):- % Human opens - X
		assert(board_sign(h,x)),
		assert(board_sign(c,o)),
		assert(board_sign(f,f)),!.
set_players(`n`):- % Computer opens - X
		assert(board_sign(h,o)),
		assert(board_sign(c,x)),
		assert(board_sign(f,f)),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create_board(N,Board)
% This rule creates an N*N sizes board.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create_col(N,Col):-
	for(X,1,N,1),
	assert(col_item(X,f)),
	fail.
create_col(N,Col):-
	findall(Item, col_item(_,Item), Col),
	retractall(col_item(_,_)).

create_board_process(N,Board,Col):-
	for(X,1,N,1),
	assert(board_item(X,Col)),	
	fail.

create_board(N,Board):-
	retractall(board_size(_)),assert(board_size(N)),
	create_col(N,Col),
	not(create_board_process(N,Board,Col)),
	findall(Item, board_item(_,Item), Board),
	retractall(board_item(_,_)),
	retractall(game(_)),assert(game(Board)).

%%%%%%%%%%%%%%%%%%%%%%%%
%% Misc Utility Rules
%%%%%%%%%%%%%%%%%%%%%%%%

% random_number(X,Lim) gets a limit Lim and gives a random
% number X which is from 1 to Lim-1
random_number(X,Lim):-X is integer(random*(Lim-1))+1.

% writeln(X) outputs X and then does nl.
writeln(X):-write(X),nl.

% respond simplifies writing a mixture of literals and variables
% Example: respond(['The value of X is: ',X])
respond([]):-
  write('.'),nl,nl.
respond([H|T]):-
  write(H),
  respond(T).

% Concates 2 lists
conc([],L,L).
conc([X|L1], L2, [X|L3]):-
	conc(L1,L2,L3).

% Finds sublist S in the list L
sublist(S,L):-
	conc(L1,L2,L),
	conc(S,L3,L2).

% add(X,L) adds X to the list L
add(X, L, [X,L]).

% atom_number(Atom,N) convers between an atom of an integer 
% Atom to an integer N.
atom_number(Atom, N):- 	string_atom(String, Atom),
			string_integer(String, N),
			not(Integer==0). 


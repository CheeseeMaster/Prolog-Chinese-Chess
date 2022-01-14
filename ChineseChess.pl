% Init

% Marshal(将)-8; Guardian(士)-9; Elephant(象)-10; Horse(马)-11; Chariot(车)-12; Cannon(砲)-13; Pawn(卒)-14
% Red
% General(帅)-1; Advisor(仕)-2; Minister(相)-3; Horse(马)-4; Chariot(车)-5;  Cannon(炮)-6; Soldiers(兵)-7; Empty(空)-0

% board(-Board).
% Creates a board container, with uninitialized positions
board(game_board(A,B,C,D,E,F,G,H,I,J)):-
	functor(A,l,9), 
	functor(B,l,9),
	functor(C,l,9),
	functor(D,l,9), 
	functor(E,l,9), 
	functor(F,l,9), 
	functor(G,l,9), 
	functor(H,l,9),
    functor(I,l,9),
    functor(J,l,9).

empty_board(
    game_board(
           l(12, 11, 10,  9,  8,  9, 10, 11, 12),
           l( 0,  0,  0,  0,  0,  0,  0,  0,  0),
           l( 0, 13,  0,  0,  0,  0,  0, 13,  0),
           l(14,  0, 14,  0, 14,  0, 14,  0, 14),
           l( 0,  0,  0,  0,  0,  0,  0,  0,  0),
           l( 0,  0,  0,  0,  0,  0,  0,  0,  0),
           l( 7,  0,  7,  0,  7,  0,  7,  0,  7),
           l( 0,  6,  0,  0,  0,  0,  0,  6,  0),
           l( 0,  0,  0,  0,  0,  0,  0,  0,  0),
           l( 5,  4,  3,  2,  1,  2,  3,  4,  5),
          )).

% board_print(+Board).
% Prints the board Board to the console.
board_print(game_board(A,B,C,D,E,F,G,H,I)):-
    tab(3), 
    print(1), tab(2),
    print(2), tab(2),
	print(3), tab(2),
    print(4), tab(2),
	print(5), tab(2),
    print(6), tab(2),
	print(7), tab(2),
    print(8), tab(2), 
    print(9), tab(2),
    nl,
	print(a), tab(2), board_print_line(A),
    print(b), tab(2), board_print_line(B),
	print(3), tab(2), board_print_line(C),
	print(4), tab(2), board_print_line(D),
	print(5), tab(2), board_print_line(E),
	print(6), tab(2), board_print_line(F),
	print(7), tab(2), board_print_line(G),
	print(8), tab(2), board_print_line(H),
    print(9), tab(2), board_print_line(I),
    print(10), tab(2), board_print_line(J).

% board_print_line(+Line).
% Auxiliary function that prints a line of the board.
board_print_line(Line):-
	board_print_line_element(Line,1),
	board_print_line_element(Line,2),
	board_print_line_element(Line,3),
	board_print_line_element(Line,4),
	board_print_line_element(Line,5),
	board_print_line_element(Line,6),
	board_print_line_element(Line,7),
	board_print_line_element(Line,8),
    board_print_line_element(Line,9),
	nl.

board_print_line_element(Line,Index):-
	arg(Index,Line,E),
	E == 0, !, 	% just a white space
	tab(3).
board_print_line_element(Line,Index):-
	arg(Index,Line,E),
	E == 1, !,
	format('~c',[]), 	% 帅 
	tab(2).
board_print_line_element(Line,Index):-
	arg(Index,Line,E),
	E == 2, !,
	format('~c',[]), 	% 仕
	tab(2).
board_print_line_element(Line,Index):-
	arg(Index,Line,E),
	E == 3, !,
	format('~c',[]),    % 相
	tab(2).
board_print_line_element(Line,Index):-
	arg(Index,Line,E),
	E == 4, !,
	format('~c',[]),    % 马
	tab(2).
board_print_line_element(Line,Index):-
	arg(Index,Line,E),
	E == 5, !,
	format('~c',[]),	% 车
	tab(2).
board_print_line_element(Line,Index):-
	arg(Index,Line,E),
	E == 6, !,
	format('~c',[]),	% 炮
	tab(2).
board_print_line_element(Line,Index):-
	arg(Index,Line,E),
	E == 7, !,
	format('~c',[]),	% 兵
	tab(2).
board_print_line_element(Line,Index):-
	arg(Index,Line,E),
	E == 8, !,
	format('~c',[]),	% 将
	tab(2).
board_print_line_element(Line,Index):-
	arg(Index,Line,E),
	E == 9, !,
	format('~c',[]),	% 士
	tab(2).
board_print_line_element(Line,Index):-
	arg(Index,Line,E),
	E == 10, !,
	format('~c',[]),	% 象
	tab(2).
board_print_line_element(Line,Index):-
	arg(Index,Line,E),
	E == 11, !,
	format('~c',[]),	% 马
	tab(2).
board_print_line_element(Line,Index):-
	arg(Index,Line,E),
	E == 12, !,
	format('~c',[]),	% 车
	tab(2).
board_print_line_element(Line,Index):-
	arg(Index,Line,E),
	E == 13, !,
	format('~c',[]),	% 砲
	tab(2).
board_print_line_element(Line,Index):-
	arg(Index,Line,E),
	E == 14, !,
	format('~c',[]),	% 卒
	tab(2).
board_print_line_element(Line,Index):-
	arg(Index,Line,E),
	print(E),		    % shouldn't reach this state
	tab(2).

% move rules

% chessboard present

% end check

% make move

change_player(Player, NextPlayer) :-
	select(Player, [red, black], Rest_Player),
	select(NextPlayer, Rest_Player, _).

print_player(red) :-
	write('red').
print_player(black) :-
	write('black').


% main
main :-
	abolish(current/2),
	chessboard_init(Board),
	assert(current(red, Board)),
	write('Prolog Chinese Chess\n'),
	write('First goes the red.\n'),
	play.

play :-
	current(Player, Board),
	chessboard_print(Board),
	make_play(Player, Board).

make_play(Player, Board) :-
	write('Red turn.\n'),
	write('Please choose the piece.\n')
	repeat,
	read(Piece),
	write('Please choose the destination.\n'),
	read(Dest),
	move(Player, Board, Piece, Dest, NewBoard),
	change_player(Player, NextPlayer),
	abolish(current/2),
	assert(current(NextPlayer, NewBoard)),
	play.
make_play(Player, Board):-
	change_player(Player, NextPlayer),
	list_available_moves(Board, NextPlayer, _),
	print_player(NextPlayer),
	write(' wins the game.\n').

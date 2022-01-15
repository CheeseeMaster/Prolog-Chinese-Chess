:-use_module(library(lists)).

% Init
% Black
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

chessboard_init(
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
           l( 5,  4,  3,  2,  1,  2,  3,  4,  5)
          )).

% board_print(+Board).
% Prints the board Board to the console.
board_print(game_board(A,B,C,D,E,F,G,H,I,J)):-
    tab(3), 
    print(1), tab(3),
    print(2), tab(3),
	print(3), tab(3),
    print(4), tab(3),
	print(5), tab(3),
    print(6), tab(3),
	print(7), tab(3),
    print(8), tab(3), 
    print(9), tab(3),
    nl,
	print(a), tab(2), board_print_line(A),
    print(b), tab(2), board_print_line(B),
	print(c), tab(2), board_print_line(C),
	print(d), tab(2), board_print_line(D),
	print(e), tab(2), board_print_line(E),
	print(f), tab(2), board_print_line(F),
	print(g), tab(2), board_print_line(G),
	print(h), tab(2), board_print_line(H),
    print(i), tab(2), board_print_line(I),
    print(j), tab(2), board_print_line(J).

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

board_print_element(0):- tab(4).
board_print_element(1):- 
	ansi_format([bold,fg(red)], '~c', [24101]),	% 帅 
	tab(2).
board_print_element(2):- 
	ansi_format([bold,fg(red)], '~c', [20181]),	% 仕 
	tab(2).
board_print_element(3):- 
	ansi_format([bold,fg(red)], '~c', [30456]),	% 相 
	tab(2).
board_print_element(4):- 
	ansi_format([bold,fg(red)], '~c', [39340]),	% 马 
	tab(2).
board_print_element(5):- 
	ansi_format([bold,fg(red)], '~c', [36554]),	% 车 
	tab(2).
board_print_element(6):-
	ansi_format([bold,fg(red)], '~c', [28846]),	% 炮 
	tab(2).
board_print_element(7):- 
	ansi_format([bold,fg(red)], '~c', [20853]),	% 兵 
	tab(2).
board_print_element(8):- 
	ansi_format([bold,fg(black)], '~c', [23559]),	% 将 
	tab(2).
board_print_element(9):- 
	ansi_format([bold,fg(black)], '~c', [22763]),	% 士 
	tab(2).
board_print_element(10):- 
	ansi_format([bold,fg(black)], '~c', [35937]),	% 象 
	tab(2).
board_print_element(11):- 
	ansi_format([bold,fg(black)], '~c', [39532]),	% 马 
	tab(2).
board_print_element(12):- 
	ansi_format([bold,fg(black)], '~c', [36710]),	% 车 
	tab(2).
board_print_element(13):- 
	ansi_format([bold,fg(black)], '~c', [30770]),	% 砲 
	tab(2).
board_print_element(14):- 
	ansi_format([bold,fg(black)], '~c', [21330]),	% 卒
	tab(2).

board_print_line_element(Line,Index):-
	arg(Index,Line,E),
	board_print_element(E).


% % helper predicate
% indexOf([Element|_], Element, 0).
% indexOf([], Element, Index):-
% 	Index is -1.
% indexOf([_|Tail], Element, Index):-
% 	indexOf(Tail, Element, Index1),
% 	Index is Index1+1.

% locate(game_board(A,B,C,D,E,F,G,H,I,J), Chess, X, Y):-
% 	locate_line(A, Chess)

% locate_line(Line, Chess,):-

% locate
pos(Board, X, Y, E):-
	arg(Y,Board,T),
	arg(X,T,E).

% divide camps
in_red(X) :- X >= 1, X < 8.
in_black(X) :- X >= 8, X < 15.

% Check if the piece is in the palace area.
in_black_center(X, Y) :-
	X > 3, X < 7,
	Y > 0, Y < 4.
in_red_center(X, Y) :-
	X > 3, X < 7,
	Y > 7, Y < 11.

% Check if the piece is in the own territory.
in_black_field(X, Y) :-
	X > 0, X < 10,
	Y > 0, Y < 6.
in_red_field(X, Y) :-
	X > 0, X < 10,
	Y > 5, Y < 11.

% Check if two generals doesn't meet.
not_meet(_, X1, _, X2, _) :-
	X1 \= X2.
not_meet(Board, X1, Y1, X2, Y2) :-
	X1 = X2,
	pos(Board, X1, Y, E),
	E \= 0,
	(Y - Y2) * (Y - Y1) < 0.

% This check whether two places can directly reach.
not_reach(Board, X1, Y1, X2, Y2) :-
	X1 = X2,
	pos(Board, X1, Y, E),
	E \= 0,
	(Y - Y2) * (Y - Y1) < 0.
not_reach(Board, X1, Y1, X2, Y2) :-
	Y1 = Y2,
	pos(Board, X, Y1, E),
	E \= 0,
	(X - X2) * (X - X1) < 0.

% This check whether two places has only one piece in between.
more_one(Board, X1, Y1, X2, Y2) :-
	X1 = X2,
	pos(Board, X1, Y3, E1),
	E1 \= 0,
	(Y3 - Y2) * (Y3 - Y1) < 0,
	pos(Board, X1, Y4, E2),
	E2 \= 0,
	(Y4 - Y2) * (Y4 - Y1) < 0,
	Y3 \= Y4.
more_one(Board, X1, Y1, X2, Y2) :-
	Y1 = Y2,
	pos(Board, X3, Y1, E1),
	E1 \= 0,
	(X3 - X2) * (X3 - X1) < 0,
	pos(Board, X4, Y1, E2),
	E2 \= 0,
	(X4 - X2) * (X4 - X1) < 0,
	X3 \= X4.

% check mate
check(Board, E, X, Y) :-
	pos(Board, OtherX, OtherY, E1),
	E1 \= 1, E1 \= 8,
	valid_step(Board, E1, OtherX, OtherY, X, Y),
	in_black(E), in_red(E1).
check(Board, E, X, Y) :-
	pos(Board, OtherX, OtherY, E1),
	E1 \= 1, E1 \= 8,
	valid_step(Board, E1, OtherX, OtherY, X, Y),
	in_red(E), in_black(E1).

% move rules
% 将
valid_step(Board, E, StartX, StartY, EndX, EndY) :-
	E = 8,
	abs(StartX - EndX) + abs(StartY - EndY) > 0,
	abs(StartX - EndX) + abs(StartY - EndY) =< 1,
	in_black_center(EndX, EndY),
	pos(Board, X, Y, 1),
	not_meet(Board, X, Y, EndX, EndY),
	\+check(Board, E, EndX, EndY).

% 帅
valid_step(Board, E, StartX, StartY, EndX, EndY) :-
	E = 1,
	abs(StartX - EndX) + abs(StartY - EndY) > 0,
	abs(StartX - EndX) + abs(StartY - EndY) =< 1,
	in_red_center(EndX, EndY),
	pos(Board, X, Y, 8),
	not_meet(Board, EndX, EndY, X, Y),
	\+check(Board, E, EndX, EndY).

% 士
valid_step(_, E, StartX, StartY, EndX, EndY) :-
	E = 9,
	abs(StartX - EndX) >= 1,
	abs(StartX - EndX) < 2,
	abs(StartY - EndY) >= 1,
	abs(StartY - EndY) < 2,
	in_black_center(EndX, EndY).

% 仕
valid_step(_, E, StartX, StartY, EndX, EndY) :-
	E = 2,
	abs(StartX - EndX) >= 1,
	abs(StartX - EndX) < 2,
	abs(StartY - EndY) >= 1,
	abs(StartY - EndY) < 2,
	in_red_center(EndX, EndY).

% 象
valid_step(Board, E, StartX, StartY, EndX, EndY) :-
	E = 10,
	abs(StartX - EndX) >= 2,
	abs(StartX - EndX) < 3,
	abs(StartY - EndY) >= 2,
	abs(StartY - EndY) < 3,
	in_black_field(EndX, EndY),
	A is abs(StartX + EndX) / 2,
	B is abs(StartY + EndY) / 2,
	pos(Board, A, B, E1),
	E1 = 0.

% 相
valid_step(Board, E, StartX, StartY, EndX, EndY) :-
	E = 3,
	abs(StartX - EndX) >= 2,
	abs(StartX - EndX) < 3,
	abs(StartY - EndY) >= 2,
	abs(StartY - EndY) < 3,
	in_red_field(EndX, EndY),
	A is abs(StartX + EndX) / 2,
	B is abs(StartY + EndY) / 2,
	pos(Board, A, B, E1),
	E1 = 0.

% 马
valid_step(Board, E, StartX, StartY, EndX, EndY) :-
	(E mod 7) >= 4,
	(E mod 7) < 5,
	abs(StartX - EndX) * abs(StartY - EndY) >= 2,
	abs(StartX - EndX) * abs(StartY - EndY) < 3,
	EndX > 0, EndX < 10,
	EndY > 0, EndY < 11,
	abs(StartX - EndX) >= 2,
	abs(StartX - EndX) < 3,
	A is abs(StartX + EndX) / 2,
	pos(Board, A, StartY, E1),
	E1 = 0.
valid_step(Board, E, StartX, StartY, EndX, EndY) :-
	(E mod 7) >= 4,
	(E mod 7) < 5,
	abs(StartX - EndX) * abs(StartY - EndY) >= 2,
	abs(StartX - EndX) * abs(StartY - EndY) < 3,
	EndX > 0, EndX < 10,
	EndY > 0, EndY < 11,
	abs(StartY - EndY) >= 2,
	abs(StartY - EndY) < 3,
	A is abs(StartY + EndY) / 2,
	pos(Board, StartX, A, E1),
	E1 = 0.

% 车
valid_step(Board, E, StartX, StartY, EndX, EndY) :-
	(E mod 7) >= 5,
	(E mod 7) < 6,
	abs(StartX - EndX) * abs(StartY - EndY) >= 0,
	abs(StartX - EndX) * abs(StartY - EndY) < 1,
	abs(StartX - EndX) + abs(StartY - EndY) > 0,
	EndX > 0, EndX < 10,
	EndY > 0, EndY < 11,
	\+not_reach(Board, StartX, StartY, EndX, EndY).

% 卒
valid_step(_, E, StartX, StartY, EndX, EndY) :-
	E = 14,
	EndX = StartX,
	EndX > 0, EndX < 10,
	EndY > 0, EndY < 11,
	EndY - StartY >= 1,
	EndY - StartY < 2.
valid_step(_, E, StartX, StartY, EndX, EndY) :-
	E = 14,
	EndX > 0, EndX < 10,
	EndY > 0, EndY < 11,
	in_red_field(StartX, StartY),
	EndY = StartY,
	abs(EndX - StartX) >= 1,
	abs(EndX - StartX) < 2.

% 兵
valid_step(_, E, StartX, StartY, EndX, EndY) :-
	E = 7,
	EndX = StartX,
	EndX > 0, EndX < 10,
	EndY > 0, EndY < 11,
	StartY - EndY >= 1,
	StartY - EndY < 2.
valid_step(_, E, StartX, StartY, EndX, EndY) :-
	E = 7,
	EndX > 0, EndX < 10,
	EndY > 0, EndY < 11,
	in_black_field(StartX, StartY),
	EndY = StartY,
	abs(EndX - StartX) >= 1,
	abs(EndX - StartX) < 2.

% 炮
valid_step(Board, E, StartX, StartY, EndX, EndY) :-
	valid_walk(Board, E, StartX, StartY, EndX, EndY);
	valid_eat(Board, E, StartX, StartY, EndX, EndY).

valid_walk(Board, E, StartX, StartY, EndX, EndY) :-
	(E mod 7) >= 6,
	(E mod 7) < 7,
	abs(StartX - EndX) * abs(StartY - EndY) >= 0,
	abs(StartX - EndX) * abs(StartY - EndY) < 1,
	abs(StartX - EndX) \= abs(StartY - EndY),
	EndX > 0, EndX < 10,
	EndY > 0, EndY < 11,
	StartX = EndX,
	\+not_reach(Board, StartX, StartY, EndX, EndY),
	pos(Board, EndX, EndY, 0).
valid_walk(Board, E, StartX, StartY, EndX, EndY) :-
	(E mod 7) >= 6,
	(E mod 7) < 7,
	abs(StartX - EndX) * abs(StartY - EndY) >= 0,
	abs(StartX - EndX) * abs(StartY - EndY) < 1,
	abs(StartX - EndX) \= abs(StartY - EndY),
	EndX > 0, EndX < 10,
	EndY > 0, EndY < 11,
	StartY = EndY,
	\+not_reach(Board, StartX, StartY, EndX, EndY),
	pos(Board, EndX, EndY, 0).

valid_eat(Board, E, StartX, StartY, EndX, EndY) :-
	(E mod 7) >= 6,
	(E mod 7) < 7,
	abs(StartX - EndX) * abs(StartY - EndY) >= 0,
	abs(StartX - EndX) * abs(StartY - EndY) < 1,
	abs(StartX - EndX) \= abs(StartY - EndY),
	pos(Board, EndX, EndY, E1),
	E1 \= 0,	
	not_reach(Board, StartX, StartY, EndX, EndY),
	\+more_one(Board, StartX, StartY, EndX, EndY).

% general check (piece at start place; must move; cannot attack its own camp)
valid_move(Board, A, [StartX|[StartY|_]], [EndX|[EndY|_]]) :- 
	valid_step(Board, A, StartX, StartY, EndX, EndY),
	pos(Board, EndX, EndY, B),
	pos(Board, StartX, StartY, A),
	in_black(A), in_red(B),
	abs(EndX - StartX) + abs(EndY - StartY) > 0.
valid_move(Board, A, [StartX|[StartY|_]], [EndX|[EndY|_]]) :- 
	valid_step(Board, A, StartX, StartY, EndX, EndY),
	pos(Board, EndX, EndY, B),
	pos(Board, StartX, StartY, A),
	in_red(A), in_black(B),
	abs(EndX - StartX) + abs(EndY - StartY) > 0.
valid_move(Board, A, [StartX|[StartY|_]], [EndX|[EndY|_]]) :- 
	valid_step(Board, A, StartX, StartY, EndX, EndY),
	pos(Board, EndX, EndY, B),
	B = 0,
	pos(Board, StartX, StartY, A),
	abs(EndX - StartX) + abs(EndY - StartY) > 0.

% chessboard present

% end check
king_alive(red, Board) :-
	once(moveLeft(Board, X)),
	valid_move(Player, game_board(A,B,C,D,E,F,G,H,I,J), [X1|Y1], [X2|Y2]).

king_alive(black, Board).
	% TODO

% make move
move(Player, game_board(A,B,C,D,E,F,G,H,I,J), NewBoard, [X1|Y1], [X2|Y2]).

move(Player, game_board(A,B,C,D,E,F,G,H,I,J), NewBoard, [X1|Y1], [X2|Y2]):-
	valid_move(Player, game_board(A,B,C,D,E,F,G,H,I,J), [X1|Y1], [X2|Y2]), 
	move_from(game_board(A,B,C,D,E,F,G,H,I,J), [X1|Y1], Target),
	move_to(game_board(A,B,C,D,E,F,G,H,I,J), [X2|Y2], NewBoard, Target).

move_from(Board, [X1|Y1], Target):-
	arg(X1, Board, Line), 
	nth0(Y1, Line, Target).

replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]):- I > -1, NI is I-1, replace(T, NI, X, R), !.

move_to(game_board(A,B,C,D,E,F,G,H,I,J), [X2|Y2], NewBoard, Target):-
	arg(X2, Board, Line), 
	functor(NewBoard, game_board, 10),
	functor(NewLine,l,9),
	replace(Y2, Line, Target, NewLine).
	arg(X2, NewBoard, NewLine).

%%%%%%%%%%%%%%%%%%%%%%%%%% [INPUT] %%%%%%%%%%%%%%%%%%%%%%%%%% 
read_input(Piece, Dest, Player, Board) :-
    repeat,
        format('~w:~n', ['Enter the piece. e.g. a1.']),
		catch(read(In_Piece), _, fail),
        (   call(check_boundary, In_Piece, StartX, StartY, Piece)
        	->  (	call(check_piece, StartX, StartY, Player, Board, E) 
					-> true, !
					;	format('ERROR: ~w~n', ['Invalid piece: empty or enemy.']), fail
				), !
			;   format('ERROR: ~w~n', ['Invalid piece. should be [a-j],[1-9].']),fail),
		board_print_element(E),
		format('is ok.~n'),
	repeat,
		format('~w:~n', ['Enter the destination. e.g. a1.']),
		catch(read(In_Dest), _, fail),
		(   call(check_boundary, In_Dest, EndX, EndY, Dest)
			->  (	call(check_dest, StartX, StartY, EndX, EndY, Player, Board, E) 
					-> true, !
					;	format('ERROR: ~w~n', ['Invalid walk.']), fail
				), !
			;   format('ERROR: ~w~n', ['Invalid dest. should be [a-j],[1-9].']),fail),
		format('~w is ok.~n', [In_Dest]).

% pos(Board, X, Y, E) :-
% 	arg(Y,Board,T),
% 	arg(X,T,E).

% is_occupied(Board,X,Y) :-
% 	pos(Board,X,Y,Element),
% 	\+number(Element).

check_piece(X, Y, Player, Board, E) :-
	pos(Board,X,Y,E),
	(Player == red -> 
		E >= 1, E =< 7;
		E >= 8, E =< 14).

check_dest(StartX, StartY, EndX, EndY, Player, Board, E) :-
	valid_step(Board, E, StartX, StartY, EndX, EndY).

check_boundary(Pos, X, Y, Value) :-
	atom_length(Pos, Len),
	Len == 2,
	atom_codes(Pos, [F, S| Tail]),
	F >= 97, F =< 106,
	S >= 49, S =< 57,
	Y is F - 96,
	X is S - 48,
	append([X],[Y], Value).

change_player(Player, NextPlayer) :-
	select(Player, [red, black], Rest_Player),
	select(NextPlayer, Rest_Player, _).

print_player(red) :-
	ansi_format([bold,fg(red)], '~w', [red]).
print_player(black) :-
	ansi_format([bold,fg(black)], '~w', [black]).

% main
main :-
	abolish(current/2),
	chessboard_init(Board),
	assert(current(red, Board)),
	write('Prolog Chinese Chess\n'),
	write('First goes the '),
	ansi_format([bold,fg(red)], '~w\n', [red]),nl,
	play.

play :-
	current(Player, Board),
	board_print(Board),
	make_play(Player, Board).

make_play(Player, Board) :-
	% king_alive(Player, Board),
	write('It\'s '),
	print_player(Player),
	write(' turn.\n'),
	read_input(Piece, Dest, Player, Board).
	move(Player, Board, NewBoard, Piece, Dest),
	change_player(Player, NextPlayer),
	write(NextPlayer),nl,
	abolish(current/2).
	assert(current(NextPlayer, NewBoard)),
	play.

make_play(Player, Board) :-
	print_player(Player),
	write(' wins the game.\n').

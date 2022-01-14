% Init

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
	write('It\'s '),
	print_player(Player),
	write(' turn.\n'),
	check_available_moves(Board, Player, _),
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
make_play(Player, Board) :-
	change_player(Player, NextPlayer),
	check_available_moves(Board, NextPlayer, _),
	print_player(NextPlayer),
	write(' wins the game.\n').

%Example of random Connect6 player
%reading stdin and writing stdout.
%(c) S. Zidek 2013

%"Compilation":
%	 swipl -q -g prolog -o xdumbo00 -c c6-dumb.pl

%CHANGELOG:
%v1: removed gtrace & starting whole game, i.e. with FIRST/START message
%v2: fixed error in get_coords - Sy2 contained semicolon
%v3: uncommented entry point
%v4: fixed asserting first stone
%v5: adapted to new SWI Prolog

:- dynamic stone/3.
:- dynamic boardRate/4.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% INPUT PARSING, OUTPUT PRINT %%%%%%%%%%%%%%

%%%%% INPUT
%Reads line from stdin, terminates on LF or EOF.
read_line(L) :-
	get_char(C),
	(isEOFEOL(C), L = [], !;
		read_line(LL), atom_codes(C,[Cd]),
		[Cd|LL] = L).

%Tests if character is EOF or LF.
isEOFEOL(C) :-
	C == end_of_file, halt; 
	(char_code(C,Code), Code==10).


get_coords1(L, X, Y) :-
	atom_codes(';', [SemiColon]),
	bagof(Isc, nth0(Isc, L, SemiColon), [Isc]), %kontroluje bodkociarku
	string_to_list(S, L), %prevedie FIRST do stringu
	atom_codes(',', [Comma]),
	bagof(Ic, nth0(Ic, L, Comma), [Icom]),
	sub_string(S, 0, Icom, _, Sx), 
	Sybeg is Icom+1,
	Sylen is Isc-Icom-1,
	sub_string(S, Sybeg, Sylen, _, Sy), %Rozparsrujeme X,Y

	string_to_list(Sx, Cx), % Prevedieme do stringu X,Y
	string_to_list(Sy, Cy),

	number_codes(X, Cx), % Ziskame cislo X
	number_codes(Y, Cy). % Ziskame cisla

get_coords(L, X1, Y1, X2, Y2) :-
	atom_codes(';', [SC]), %overujeme bodkociarku
	bagof(Isc, nth0(Isc, L, SC), SemiColons),
	SemiColons = [SC1, _SC2],
	string_to_list(S, L),

	%parsrovanie parametrov STONES
	sub_string(S, 0, SC1, _, S1),
	string_to_list(S1, L1),
	atom_codes(',', [Comma]),
	bagof(Ic1, nth0(Ic1, L1, Comma), [Icom1]),
	sub_string(S1, 0, Icom1, _, Sx1), 
	Sy1beg is Icom1+1,
	sub_string(S1, Sy1beg, _, 0, Sy1),

	S2beg is SC1+1,
	sub_string(S, S2beg, _, 0, S2),
	string_to_list(S2, L2),
	bagof(Ic2, nth0(Ic2, L2, Comma), [Icom2]),
	sub_string(S2, 0, Icom2, _, Sx2), 
	Sy2beg is Icom2+1,
	sub_string(S2, Sy2beg, _, 1, Sy2),

	%prevod parametrov STONES
	string_to_list(Sx1, Cx1),
	string_to_list(Sy1, Cy1),
	string_to_list(Sx2, Cx2),
	string_to_list(Sy2, Cy2),

	%ziskanie cisiel
	number_codes(X1, Cx1),
	number_codes(Y1, Cy1),
	number_codes(X2, Cx2),
	number_codes(Y2, Cy2).

%%%%% OUTPUT

% zapis stones na obrazovku
write_stones(X1, Y1, X2, Y2) :-
	number_codes(X1, Sx1),
	number_codes(Y1, Sy1),
	number_codes(X2, Sx2),
	number_codes(Y2, Sy2),

	append(["STONES:", Sx1, ",", Sy1, ";", Sx2, ",", Sy2, ";"], L),
	put_line(L).

put_line(L) :-
	writef('%s\n', [L]).

%%%%% -------- INPUT PARSING, OUTPUT PRINT ------------- %%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
%%%%% NOT USED YET %%%%%%%%%%%%%%
%	
%gen_pos(S, X, Y) :-
%	X is random(S) + 1,
%	Y is random(S) + 1.
%
%gen_random_free(X, Y) :-
%	board_size(S),
%	%TODO: check if board is full
%	repeat,
%	gen_pos(S, X, Y),
%	\+ stone(_, X, Y).
%
%move1(X, Y) :-
%	gen_random_free(X, Y),
%	assert(stone(0, X, Y)).
%	
%move(X1, Y1, X2, Y2) :-
%	move1(X1, Y1),
%	move1(X2, Y2).
%

%check_i_am_neighboor(Player, (SX:SY),(X, Y)) :- not(stone(Player, SX , SY)), SY =:= Y - 1,(SX =:= X - 1; SX =:= X + 1; SX == X);
%						not(stone(Player, SX , SY)), SY =:= Y ,(SX =:= X - 1; SX =:= X + 1; SX == X);
%						not(stone(Player, SX , SY)), SY =:= Y + 1,(SX =:= X - 1; SX =:= X + 1; SX == X).
%

%%%%% -------- NOT USED YET ------------- %%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% GENERATE FIRST MOVES %%%%%%%%%%%%%%

gen_stone_after_first(X,Y,X1,Y1,X2,Y2) :- 
	X >= 1, X < 10, Y >= 1, Y =< 10, X1 is X + 1, Y1 is Y, X2 is X + 1, Y2 is Y + 1,!;
	X > 10, X =< 19, Y >= 1, Y =< 10, X1 is X - 1, Y1 is Y, X2 is X - 1, Y2 is Y + 1,!;
	X >= 1, X < 10, Y > 10, Y =< 19, X1 is X + 1, Y1 is Y, X2 is X + 1, Y2 is Y - 1,!;
	X > 10, X =< 19, Y > 10, Y =< 19, X1 is X - 1, Y1 is Y, X2 is X - 1, Y2 is Y - 1,!;
	X == 10, Y =< 10,Y >= 1, X1 is X, Y1 is Y + 1, X2 is X - 1, Y2 is Y + 1,!;
	X == 10, Y > 10,Y =< 19, X1 is X, Y1 is Y - 1, X2 is X + 1, Y2 is Y - 1.
	
gen_stones_after_start(X1,Y1,X2,Y2) :-
	not(is_occupied_horizontal(5)), X1 is 9, X2 is 11, Y1 is 10, Y2 is 10,!;
	not(is_occupied_vertical(5)), X1 is 10, X2 is 10, Y1 is 9, Y2 is 11,!;
	not(is_occupied_diag_lr(5)), X1 is 9, X2 is 11, Y1 is 9, Y2 is 11,!;
	not(is_occupied_diag_rl(15, 5)), X1 is 11, X2 is 9, Y1 is 9, Y2 is 11.
		
is_occupied_horizontal(X) :- stone(1, X, 10), true; X1 is X + 1, X1 =< 15, is_occupied_horizontal(X1).
is_occupied_vertical(Y) :- stone(1, 10, Y), true; Y1 is Y + 1, Y1 =< 15, is_occupied_vertical(Y1).
is_occupied_diag_lr(D) :- stone(1, D, D), true; D1 is D + 1, D1 =< 15, is_occupied_diag_lr(D1).
is_occupied_diag_rl(X,Y) :- stone(1, X, Y), true; X1 is X - 1, Y1 is Y + 1, Y1 =< 15, is_occupied_diag_rl(X1, Y1).

%%%%% -------- GENERATE FIRST MOVES ------------- %%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% INPUT CORRECTNES %%%%%%%%%%%%%%

is_out(X, Y) :- X < 1,!; X > 19,!; Y < 1,!; Y > 19,!.
is_same(X0,Y0,X1,Y1) :- X0 == X1,!, Y0 == Y1,!. 

%%%%% -------- INPUT CORRECTNES ------------- %%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% OPERATION WITH STONES, ASSERT, FIND NEIGHBOORS ... %%%%%%%%%%%%%%

assert_stones(Player, X1, Y1, X2, Y2) :-
	assert(stone(Player, X1, Y1)),
	assert(stone(Player, X2, Y2)).

check_stone_on_list(_, [], L, L).
check_stone_on_list(Player, [(X,Y)|T], Z , L) :- (not(stone(_,X, Y)), not(boardRate(Player, X, Y, _))) -> check_stone_on_list(Player, T, [X:Y|Z], L); (check_stone_on_list(Player, T, Z, L)),!. 
	
get_neighboors(Player, X,Y, L) :-
	XR is X +1, XL is X - 1, YU is Y - 1, YD is Y + 1,
	T = [(XR, YU), (XR, Y), (XR, YD), (X, YD), (XL, YD), (XL, Y), (XL, YU), (X, YU)],
	check_stone_on_list(Player, T,[], L).
	
find_all_stones(Player, L) :-
	findall((SX:SY), stone(Player, SX, SY), L).
	
call_neighboors(_, []).
call_neighboors(Player, [X:Y|L]) :- get_neighboors(Player, X, Y, N), calculate_rate(Player, N), call_neighboors(Player, L),!.

%%%%% -------- OPERATION WITH STONES, ASSERT, FIND NEIGHBOORS ------------- %%%%%%%%%%%%%%
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% DIRECTION THINGS %%%%%%%%%%%%%%	

getLineLengthInDirection(Player, X, Y, Direction, Length) :-
	not(stone(Player, X, Y)) -> Length is 0 ;
	(
		Direction == 1, X2 is X + 1, X2 =< 19, (stone(Player, X2, Y) -> (getLineLengthInDirection(Player, X2, Y, 1, Length2), Length is Length2+1) ; Length is 1),!;
		Direction == 2, X2 is X + 1, Y2 is Y + 1, X2 =< 19, Y2 =< 19, (stone(Player, X2, Y2) -> (getLineLengthInDirection(Player, X2, Y2, 2, Length2), Length is Length2+1) ; Length is 1),!;
		Direction == 3, Y2 is Y + 1, Y2 =< 19, (stone(Player, X, Y2) -> (getLineLengthInDirection(Player, X, Y2, 3, Length2), Length is Length2+1) ; Length is 1),!;
		Direction == 4, X2 is X - 1, Y2 is Y + 1, X2 >= 1, Y2 =< 19, (stone(Player, X2, Y2) -> (getLineLengthInDirection(Player, X2, Y2, 4, Length2), Length is Length2+1) ; Length is 1),!;
		Direction == 5, X2 is X - 1, X2 >= 1, (stone(Player, X2, Y) -> (getLineLengthInDirection(Player, X2, Y, 5, Length2), Length is Length2+1) ; Length is 1),!;
		Direction == 6, X2 is X - 1, Y2 is Y - 1, X2 >= 1, Y2 >= 1, (stone(Player, X2, Y2) -> (getLineLengthInDirection(Player, X2, Y2, 6, Length2), Length is Length2+1) ; Length is 1),!;
		Direction == 7, Y2 is Y - 1, Y2 >= 1, (stone(Player, X, Y2) -> (getLineLengthInDirection(Player, X, Y2, 7, Length2), Length is Length2+1) ; Length is 1),!;
		Direction == 8, X2 is X + 1, Y2 is Y - 1, X2 =< 19, Y2 >= 1, (stone(Player, X2, Y2) -> (getLineLengthInDirection(Player, X2, Y2, 8, Length2), Length is Length2+1) ; Length is 1),!;
		Length is 1
	).

getLineLengthAndDirection(Player, X, Y, Direction, Length) :-
	(X < 1 ; X > 19 ; Y < 1 ; Y > 19) -> (Direction is -1, Length is -1, false) ;
	(
		getLineLengthInDirection(Player, X, Y, 1, LengthHor1),
		getLineLengthInDirection(Player, X, Y, 5, LengthHor2),
		LengthHor is LengthHor1 + LengthHor2 - 1,
		getLineLengthInDirection(Player, X, Y, 2, LengthDiag11),
		getLineLengthInDirection(Player, X, Y, 6, LengthDiag12),
		LengthDiag1 is LengthDiag11 + LengthDiag12 - 1,
		getLineLengthInDirection(Player, X, Y, 3, LengthVert1),
		getLineLengthInDirection(Player, X, Y, 7, LengthVert2),
		LengthVert is LengthVert1 + LengthVert2 - 1,
		getLineLengthInDirection(Player, X, Y, 4, LengthDiag21),
		getLineLengthInDirection(Player, X, Y, 8, LengthDiag22),
		LengthDiag2 is LengthDiag21 + LengthDiag22 - 1,
		(
			LengthHor >= LengthVert, LengthHor >= LengthDiag1, LengthHor >= LengthDiag2, Length is LengthHor, Direction is 1,!;
			LengthDiag1 > LengthVert, LengthDiag1 > LengthHor, LengthDiag1 >= LengthDiag2, Length is LengthDiag1, Direction is 2,!;
			LengthVert > LengthHor, LengthVert >= LengthDiag1, LengthVert >= LengthDiag2, Length is LengthVert, Direction is 3,!;
			LengthDiag2 > LengthVert, LengthDiag2 > LengthHor, LengthDiag2 > LengthDiag1, Length is LengthDiag2, Direction is 4
		)
	).

getPotentialLineLengthAndDirection(Player, X, Y, Direction, Length) :-
	(X < 1 ; X > 19 ; Y < 1 ; Y > 19) -> (Direction is -1, Length is -1, false) ;
	(
		Xl is X - 1,
		Xr is X + 1,
		Yu is Y - 1,
		Yb is Y + 1,
		getLineLengthInDirection(Player, Xr, Y, 1, LengthHor1),
		getLineLengthInDirection(Player, Xl, Y, 5, LengthHor2),
		LengthHor is LengthHor1 + LengthHor2 + 1,
		getLineLengthInDirection(Player, Xr, Yb, 2, LengthDiag11),
		getLineLengthInDirection(Player, Xl, Yu, 6, LengthDiag12),
		LengthDiag1 is LengthDiag11 + LengthDiag12 + 1,
		getLineLengthInDirection(Player, X, Yb, 3, LengthVert1),
		getLineLengthInDirection(Player, X, Yu, 7, LengthVert2),
		LengthVert is LengthVert1 + LengthVert2 + 1,
		getLineLengthInDirection(Player, Xl, Yb, 4, LengthDiag21),
		getLineLengthInDirection(Player, Xr, Yu, 8, LengthDiag22),
		LengthDiag2 is LengthDiag21 + LengthDiag22 + 1,
		(
			LengthHor >= LengthVert, LengthHor >= LengthDiag1, LengthHor >= LengthDiag2, Length is LengthHor, Direction is 1,!;
			LengthDiag1 > LengthVert, LengthDiag1 > LengthHor, LengthDiag1 >= LengthDiag2, Length is LengthDiag1, Direction is 2,!;
			LengthVert > LengthHor, LengthVert >= LengthDiag1, LengthVert >= LengthDiag2, Length is LengthVert, Direction is 3,!;
			LengthDiag2 > LengthVert, LengthDiag2 > LengthHor, LengthDiag2 > LengthDiag1, Length is LengthDiag2, Direction is 4
		)
	).
	
%%%%% -------- DIRECTION THINGS ------------- %%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% RATE THINGS %%%%%%%%%%%%%%

saveRateAt(Player, X, Y, Rate) :-
	boardRate(Player, X, Y, _) -> put_line("Error: Rate for the position has been saved already!"),! ;
	(
		assert(boardRate(Player, X, Y, Rate))
	).

removeRates(Player) :-
	retractall(boardRate(Player,_,_,_)).

selectBestRatePosition([X:Y:R],X,Y,R).
selectBestRatePosition([X:Y:R|XS],X,Y,R):- selectBestRatePosition(XS,_,_,R2), R >= R2, !.
selectBestRatePosition([_:_:R|XS],X2,Y2,R2):- selectBestRatePosition(XS,X2,Y2,R2), R2 > R, !.

getBestRatePosition(Player, X, Y, Rate) :-
	findall(Xb:Yb:Rb,boardRate(Player,Xb,Yb,Rb),Rates), selectBestRatePosition(Rates, X, Y, Rate).

calculate_rate(_, []).
calculate_rate(Player, [X:Y|L]) :- getPotentialLineLengthAndDirection(Player, X, Y, _, RATE), saveRateAt(Player, X, Y, RATE), calculate_rate(Player, L),!.	

%%%% RATE
rate(Player) :- find_all_stones(Player, L), call_neighboors(Player, L).
	
%%%%% -------- RATE THINGS ------------- %%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% MINIMAX %%%%%%%%%%%%%%

minimax :- 
	removeRates(0),
	removeRates(1),
	rate(0),
	rate(1),
	getBestRatePosition(0, X0R, Y0R, R0),
	getBestRatePosition(1, X1R, Y1R, R1),
	%write(X0R), write(" "), write(Y0R), write(" "), write(R0), write(" "), nl,
	((R0 >= R1) -> (X1 is X0R, Y1 is Y0R); (X1 is X1R, Y1 is Y1R)),		
	%write(X1), write(" "), write(Y1), write(" "), nl,		
	assert(stone(0, X1, Y1)),
	removeRates(0),
	removeRates(1),
	%listing(stone),
	%listing(boardRate),
	rate(0),		
	rate(1),
	getBestRatePosition(0, X0R2, Y0R2, R02),
	getBestRatePosition(1, X1R2, Y1R2, R12),		
	((R02 > R12) -> (X2 is X0R2, Y2 is Y0R2); (X2 is X1R2, Y2 is Y1R2)),
	assert(stone(0, X2, Y2)),
	write_stones(X1, Y1, X2, Y2).	
	
%%%%% -------- MINIMAX ------------- %%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% !!!!!!!!!!!MAIN!!!!!!!!!! %%%%%%%%%%%%%%

start :-
	read_line(L),
	(
		atom_codes('FIRST:', AC), % Zaciname s FIRST, prvy suter davame my
		append(AC, CS, L),
		get_coords1(CS, X, Y),
		((is_out(X, Y)) -> put_line("Error, Stone is out of play array! Please re-enter your first move."), start
			; (assert(stone(1, X, Y)),		
			gen_stone_after_first(X,Y,X1,Y1,X2,Y2),
			write_stones(X1, Y1, X2, Y2),
			assert_stones(0, X1, Y1, X2, Y2),
			play)
		)
	;
		L = "START;", % Zacina prvym on
		assert(stone(0, 10, 10)),
		put_line("FIRST:10,10;"),
		after_start
	;
		halt
	).
	
after_start :-
	read_line(L),
	(
		atom_codes('QUIT;', AC),
		L == AC, halt
	;
		atom_codes('STONES:', AC), % parsujeme STONES
		append(AC, CS, L),
		get_coords(CS, X1o, Y1o, X2o, Y2o),
		((is_same(X1o, Y1o, X2o, Y2o)) -> (put_line("Error, Try to save both stones at same place! Please re-enter your stone move!"), after_start)
			;
			((is_out(X1o, Y1o); stone(_, X1o, Y1o)) -> 
				(put_line("Error, First stone already exists or is out of play array! Please re-enter your stone move!"), after_start)
			;
				((is_out(X2o, Y2o); stone(_, X2o, Y2o)) -> 
					(put_line("Error, Second stone already exists or is out of play array! Please re-enter your stone move!"), after_start)
				;
					(assert(stone(1, X1o, Y1o)),
					assert(stone(1, X2o, Y2o)),
					gen_stones_after_start(X1, Y1, X2, Y2),		
					write_stones(X1, Y1, X2, Y2),
					assert_stones(0, X1, Y1, X2, Y2),
					play)
				)
			)
		)
	;
		halt
	).
	
play :-
	put_line("play!"),
	read_line(L),
	(
		atom_codes('QUIT;', AC),
		L == AC, halt
	;
		atom_codes('STONES:', AC), % parsujeme STONES
		append(AC, CS, L),
		get_coords(CS, X1o, Y1o, X2o, Y2o),
		((is_same(X1o, Y1o, X2o, Y2o)) -> (put_line("Error, Try to save both stones at same place! Please re-enter your stone move!"), play)
			;
			((is_out(X1o, Y1o); stone(_, X1o, Y1o)) -> 
				(put_line("Error, First stone already exists or is out of play array! Please re-enter your stone move!"), play)
			;
				((is_out(X2o, Y2o); stone(_, X2o, Y2o)) -> 
					(put_line("Error, Second stone already exists or is out of play array! Please re-enter your stone move!"), play)
				;
					(assert(stone(1, X1o, Y1o)),
					assert(stone(1, X2o, Y2o)),
					minimax,
					play)
				)
			)
		)		
	;
		halt
	).

%entry point
prolog :-
	prompt(_, ''),
	start.
	
%%%%% -------- !!!!!!!!END OF MAIN!!!!!!!!! ------------- %%%%%%%%%%%%%%



%% tento soubor m�ete ve sv�ch projektech libovoln� pou��t
%% PS: minul� rok bylo jedn�m studentem vytvo�eno grafick� rozhran�,
%%     ke sta�en� na https://gist.github.com/izidormatusov/5114798
%% PS2: zejm�na p�i pou��v�n� r�zn�ch knihoven si d�vejte dobr� pozor,
%%     zda je po�adovan� funkce dostupn� na referen�n�m serveru
%% M. Hyr�, 21.3.2014

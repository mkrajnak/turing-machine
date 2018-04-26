/** non deterministic Turing machine simulation, project for FLP course at FIT BUT
 Martin Krajnak, xkrajn02@stud.fit.vurbr.cz
*/
:- initialization(main, main).

inkrementuj(X,Y) :- Y is X+1.

first([],_).
first([X|_],Y) :- Y = X.

empty([]) :- true.
empty([_|_]) :- false.


main(Argv) :-
  first(Argv, Y),
  (Y == [],
    prompt(_, ''),
    read_lines(LL),!;
    open(Y, read, Stream),
    read_lines(Stream, LL)
  ),
  write(LL);
  split_lines(LL, S),
  write(S),
  halt.

% Reads line from stdin, terminates on LF or EOF.
read_line(L,C) :-
  get_char(C),
  (isEOFEOL(C), L = [], !;
		read_line(LL,_),% atom_codes(C,[Cd]),
		[C|LL] = L).

read_line(Stream,L,C) :-
  get_char(Stream,C),
  (isEOFEOL(C), L = [], !;
		read_line(Stream,LL,_),% atom_codes(C,[Cd]),
		[C|LL] = L).


% Tests if character is EOF or LF.
isEOFEOL(C) :-
	C == end_of_file;
	(char_code(C,Code), Code==10).

% Read file
read_lines(Stream,Ls) :-
	read_line(Stream,L,C),
	( C == end_of_file, Ls = [] ;
	  read_lines(Stream,LLs), Ls = [L|LLs]
	).

% Read lines
read_lines(Ls) :-
	read_line(L,C),
	( C == end_of_file, Ls = [] ;
	  read_lines(LLs), Ls = [L|LLs]
	).


% rozdeli radek na podseznamy
split_line([],[[]]) :- !.
split_line([' '|T], [[]|S1]) :- !, split_line(T,S1).
split_line([32|T], [[]|S1]) :- !, split_line(T,S1).    % aby to fungovalo i s retezcem na miste seznamu
split_line([H|T], [[H|G]|S1]) :- split_line(T,[G|S1]). % G je prvni seznam ze seznamu seznamu G|S1

% vstupem je seznam radku (kazdy radek je seznam znaku)
split_lines([],[]).
split_lines([L|Ls],[H|T]) :- split_lines(Ls,T), split_line(L,H).

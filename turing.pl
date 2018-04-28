/** non deterministic Turing machine simulation, project for FLP course at FIT BUT
 Martin Krajnak, xkrajn02@stud.fit.vurbr.cz
*/

first([],_).
first([X|_],Y) :- Y = X.

isLastElem([X|[]],E) :- E == X, !.
isLastElem([_|T],E) :- isLastElem(T,E).

:- dynamic rule/1.

main(Argv) :-
  first(Argv, Y),
  (Y == [],
    prompt(_, ''),
    read_lines(LL),!;
    open(Y, read, Stream),
    read_lines(Stream, LL)
  ),
  split_lines(LL, S),
  getTape(S, Tape),
  delete(S,Tape,Rules),
  addTapeBound(Tape, NTape),
  addRules(Rules),
  % forall(rule(R),writeln(R)),
  makeTransitions('S', NTape, 2, Result),
  printResults(Result),
  halt.

printResult([]).
printResult([H|T]) :-
  H == '<', printResult(T), !;
  T == ['>'], !;
  write(H),
  printResult(T).


printResults([]).
printResults([H|T]) :-
  printResult(H),
  writeln(""),
  printResults(T).

addTapeBound([H|_], NTape) :-
  append(['<'],H,X),
  append(X,['>'],NTape).


insertStateToTape(_, [], _, _) :- !.
insertStateToTape(State, [H|T], Pos, Res) :-
  Pos == 1, append([State], [H|T], Res), !;
  NPos is Pos-1,
  insertStateToTape(State, T, NPos, NewTape),
  Res=[H|NewTape].


writeToPos(_, _, [], _).
writeToPos(Symbol, Pos, [S|T], Res) :-
  Pos == 1, Res=[Symbol|T], !;
  NPos is Pos-1,
  writeToPos(Symbol, NPos, T, NewTape), Res=[S|NewTape].


takeAction(Tape, Pos, [_, _, Ns, Ac], NTape, NPos, Ns) :-
  Ac == 'R', NTape=Tape, NPos is Pos+1, !;
  Ac == 'L', NTape=Tape, NPos is Pos-1, !;
  NPos = Pos, writeToPos(Ac, Pos, Tape, NTape).


getSymbol([], _, _).
getSymbol([H|T], Pos, Res) :-
  Pos == 1, Res=H,!;
  NPos is Pos-1,
  getSymbol(T, NPos, Res).


makeTransitions(State, Tape, Pos , Res) :-
  insertStateToTape(State, Tape, Pos, Cfg),
  getSymbol(Tape, Pos, Sy),
  getRule(State, Sy, Rule),
  takeAction(Tape, Pos, Rule, NTape, NPos, NState),
  (
  NState == 'F', Res=[Cfg], !;
  makeTransitions(NState, NTape, NPos , Cfgs), Res=[Cfg|Cfgs]
  ).

% find a rule for current state and tape symbol
getRule(State, Symbol, Rule) :-
  rule([State,Symbol,Next,Ta]),
  Rule = [State,Symbol,Next,Ta].


getTape([R|RT],S) :-
  isLastElem([R|RT], R), S=R, ! ;
  getTape(RT,S).


getRule([[H1|_],[H2|_],[H3|_],[H4|_]], [H1, H2, H3, H4]).

% add turing machine rules to a database
addRules([]).
addRules([R|T]) :-
  getRule(R, NR),
  assertz(rule(NR)),
  addRules(T).

% prints current state, symbol, next state, tape action
printRules([]).
printRules([H|T]) :-
  writeln(H),
  printRules(T).

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

/** non deterministic Turing machine simulation, project for FLP course at FIT BUT
 Martin Krajnak, xkrajn02@stud.fit.vurbr.cz
*/
:- initialization(main, main).

inkrementuj(X,Y) :- Y is X+1.

first([],_).
first([X|_],Y) :- Y = X.

empty([]) :- true.
empty([_|_]) :- false.

getline(end_of_file, _, _) :- !.
getline('\n',_, _) :- !.
getline(C, Stream, X) :-
  write(C),
  X = [C|X],
  get_char(Stream, C2),
  write(C2),
  getline(C2, Stream, X).


main(Argv) :-
  % first(Argv, X),
  % writeln(X).
  first(Argv, Y),
  open(Y, read, Stream),
  get_char(Stream, C),
  getline(C, Stream, X),
  write(X),
  close(Stream).

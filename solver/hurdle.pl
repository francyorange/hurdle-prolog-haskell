#!/usr/bin/env swipl
% A solver for the Hurdle word game
% https://playhurdle.vercel.app/

:-['wordlist'].

main(Filename) :-
  % Filename is a test file name in quotes
  load(Filename),
  findall(Solution, solve(Solution), Sols),
  sort(Sols, SortedSols),
  writeOut(SortedSols).

solve(Solution) :-
  word(Solution),
  testGuesses(Solution).

% remove exact match
remove_exact([], [], [], []).
remove_exact([X|L1], [X|L2], Res1, Res2) :-  
  remove_exact(L1, L2, Res1, Res2).
remove_exact([X|L1], [Y|L2], [X|Res1], [Y|Res2]) :- 
  Y \= X,
  remove_exact(L1, L2, Res1, Res2).

% remove the first encounterd element
remove_first(X, [], []).
remove_first(X, [X|L1], L1).
remove_first(X, [Y|L1], [Y|Res]) :-
  Y \= X, 
  remove_first(X, L1, Res).

% Count exact match
count_exact([], [], 0).
count_exact([H|T1], [H|T2], Exact) :- 
  count_exact(T1, T2, NewExact),
  Exact is NewExact + 1.
count_exact([H|T1], [X|T2], Exact) :- 
  H \= X,
  count_exact(T1, T2, Exact).
  
% Count inexact match
count_intersection([], _, 0).
count_intersection([H|L1], L2, Count) :-
  not(member(H, L2)),
  count_intersection(L1, L2, Count).
count_intersection([H|L1], L2, Count) :-
  member(H, L2),
  remove_first(H, L2, Res),
  count_intersection(L1, Res, NewCount),
  Count is NewCount + 1.

count_inexact(Guess, Solution, Inexact) :- 
  remove_exact(Guess, Solution, GuessRes, SolutionRes),
  count_intersection(GuessRes, SolutionRes, Inexact).

guess(Guess, Exact, Inexact, Solution) :-
  count_exact(Guess, Solution, Exact),
  count_inexact(Guess, Solution, Inexact).


% Utility predicates
% You won't need to modify anything
% below this point, but you should
% read it and make sure you understand
% what it all does.

% Write out solutions
writeOut([]) :-
  write('No more solutions found.').
writeOut([Solution|Rest]) :-
  write("Possible solution: '"),
  writeOut_(Solution),
  write("'"), nl,
  writeOut(Rest).

writeOut_([]).
writeOut_([Letter|Rest]) :-
  write(Letter),
  writeOut_(Rest).

% Declare testGuesses/1 to be dynamic to
% suppress a "redefined" warning. 
:- dynamic testGuesses/1.
% Consult the file and check that
% the expected predicates are defined
% (You'll need to define guess/4 yourself)
load(Filename) :-
  [Filename],
  current_predicate(word/1),
  current_predicate(testGuesses/1),
  current_predicate(guess/4).

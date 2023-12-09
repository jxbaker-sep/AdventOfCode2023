:- module(primes, [prime/1, roots/2, lcm_list/2]).
:- use_module(utils, [product/2]).
:- dynamic known_prime/1.

known_prime(2).
known_prime(3).

prime(X) :- known_prime(X).
prime(X) :- 
  var(X),
  generate_primes(X).

prime(X) :-
  \+ var(X),
  generate_primes_to(X), !,
  known_prime(X), !.

generate_primes(X) :- generate_prime(X); generate_primes(X).

generate_primes_to(X) :-
  \+ var(X),
  findall(Prime, known_prime(Prime), KnownPrimes),
  max_list(KnownPrimes, Max),
  X #=< Max, !.

generate_primes_to(X) :-  
  generate_prime(_), !,
  generate_primes_to(X), !.

every(_, []) :- !.
every(Goal, [H|T]) :-
  !, call(Goal, H), every(Goal, T).

generate_prime(X) :-
  findall(Prime, known_prime(Prime), KnownPrimes),
  max_list(KnownPrimes, Max),
  Candidate #= Max + 2,
  generate_prime(KnownPrimes, Candidate, X).
  
generate_prime(KnownPrimes, Candidate, X) :-
  every([P]>>((Candidate mod P) #\= 0), KnownPrimes),
  assert(( known_prime(Candidate) )),
  X = Candidate, !.

generate_prime(KnownPrimes, Candidate, X) :-
  Candidate2 #= Candidate + 2,
  generate_prime(KnownPrimes, Candidate2, X), !.

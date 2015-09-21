adjacent(R,C,Ar,Ac,_) :-
 Ar is R -1,
 Ar >= 1,
 Ac is C.
adjacent(R,C,Ar,Ac,N) :-
 Ar is R + 1,
 Ar =< N,
 Ac is C.
adjacent(R,C,Ar,Ac,_) :-
 Ac is C - 1,
 Ac >= 1,
 Ar is R.
adjacent(R,C,Ar,Ac,N) :-
 Ac is C + 1,
 Ac =< N,
 Ar is R.

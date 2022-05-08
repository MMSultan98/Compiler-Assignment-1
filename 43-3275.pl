s(T, A, Z) :- sentence(T, A, Z).
sentence(s(NP,VP), A, Z) :- noun_phrase(NP, A, B), verb_phrase(VP, B, Z).

noun_phrase(np(D,N), A, Z) :- det(D, A, B), noun(N, B, Z).

verb_phrase(vp(V,NP), A, Z) :- verb(V, A, B), noun_phrase(NP, B, Z).

det(d(D), [D|X], X).

noun(n(N), [N|X], X).

verb(v(V), [V|X], X).


% Lexicon
d(the).
d(a).

n(cat).
n(bat).
n(boy).

v(eats).
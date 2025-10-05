:- module(adapter, [
  is_channel/2, is_intent/2, has_disclosure/2, has_claim/2,
  get_attr/3, attr/3, scenario/2
]).

% ---------- helpers

value_match(V, E) :-
    ( var(V) -> fail
    ; V == E
    ; string(V), atom(E), atom_string(E, S), V == S
    ; atom(V), string(E), atom_string(V, S2), S2 == E
    ).

list_has(E, L) :-
    is_list(L),
    member(X, L),
    value_match(X, E).

% ---------- generic getter across possible fact shapes

get_attr(S,K,V) :-
    % attr/3
    ( current_predicate(user:attr/3),            user:attr(S,K,V) ) ;
    % K(S,V)
    ( atom(K), current_predicate(user:K/2),
      T2 =.. [K,S,V], call(user:T2) ) ;
    % K(S) -> true
    ( atom(K), current_predicate(user:K/1),
      T1 =.. [K,S],  call(user:T1), V = true ) ;
    % fact(S, K(V))
    ( current_predicate(user:fact/2),
      T =.. [K,V], user:fact(S,T) ) ;
    % scenario(S, K(V))
    ( current_predicate(user:scenario/2),
      T =.. [K,V], user:scenario(S,T) ) ;
    % prop/property
    ( current_predicate(user:prop/3),            user:prop(S,K,V) ) ;
    ( current_predicate(user:property/3),        user:property(S,K,V) ).

attr(S,K,V) :- get_attr(S,K,V).

scenario(S,Term) :-
    Term =.. [K,V],
    get_attr(S,K,V).

% ---------- typed adapters with normalization

is_channel(S,Expected) :-
    ( current_predicate(user:channel/2), user:channel(S,V), value_match(V,Expected) )
    ;
    ( get_attr(S, channel, V2), value_match(V2, Expected) ).

is_intent(S,Expected)  :-
    ( current_predicate(user:message_intent/2),  user:message_intent(S,V), value_match(V,Expected) )
    ;
    ( Expected = cem,           current_predicate(user:is_cem/1),           user:is_cem(S) )
    ;
    ( Expected = transactional, current_predicate(user:is_transactional/1), user:is_transactional(S) )
    ;
    ( get_attr(S, message_intent, V2), value_match(V2, Expected) ).

has_disclosure(S,D) :-
    ( current_predicate(user:has_disclosure/2),  user:has_disclosure(S,V), value_match(V,D) )
    ;
    ( get_attr(S, disclosures, L), list_has(D, L) ).

has_claim(S,C) :-
    ( current_predicate(user:has_claim/2),       user:has_claim(S,V), value_match(V,C) )
    ;
    ( current_predicate(user:claim_types/2),     user:claim_types(S,L), list_has(C, L) )
    ;
    ( get_attr(S, claim_types, L2), list_has(C, L2) ).

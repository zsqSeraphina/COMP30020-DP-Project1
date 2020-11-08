% Author: Siqi Zhou <szhou7@student.unimelb.edu.au>
% Student Number: 903274

%----------------------------------------------------------------------------%
% This project is aim to play part of the cribbage game
% by implimenting the hand_value(Hand, Startcard, Value) 
% which counts the value of one players' cards
% and select_hand(Cards, Hand, Cribcards) 
% which keeps the cards of a player 
%that on average will result a highest value

% card(Rank, Suit) is the expression of a card
% Rank is one of [ace,2,3,4,5,6,7,8,9,10,jack,queen,king] ordered as the index
% Suit is one of [clubs, diamonds, hearts, spades] with no order
%----------------------------------------------------------------------------%

:- use_module(library(clpfd)).

%----------------------------------------------------------------------------%
% First part purpose: 
% implement the functionality of counting the value of cards in hand
%----------------------------------------------------------------------------%
% This part implements sorting a list of cards
%----------------------------------------------%

% card_rank_index(card(A,_), Rank)
% true when I is the index of A in cards' order
card_rank_index(card(A,_), I) :-
    nth0(I, [ace,2,3,4,5,6,7,8,9,10,jack,queen,king], A).

% card_cmp(O, A, B) compares the 2 cards with ranks A, B
% and gives the resulting sign O
card_cmp(O, card(A,_), card(B,_)) :-
    A == B;
    card_rank_index(card(A,_), X),
    card_rank_index(card(B,_), Y),
    compare(O, X, Y).

% card_sort(Cards, Sorted) sorts the Cards into ascending order list Sorted
card_sort(Cards, Sorted) :- 
    predsort(card_cmp, Cards, Sorted),!.

%--------------------------------------------------------%
% This part implements some help functions for later use
%--------------------------------------------------------%

% replace_rank(A, B) true when B is a list of cards same as A 
%except all the ranks has been converted into numbers as the rule required
replace_rank([], []).
replace_rank([card(A, _)|Cards], [B|Converted]) :-
    member(A, [jack, queen, king]) ->
    B = 10,
    replace_rank(Cards, Converted)
    ;A = ace ->
    B = 1,
    replace_rank(Cards, Converted)
    ;B = A, 
    replace_rank(Cards, Converted).

% find_card_groups(Cards, Found]) true when Found is a sublist of Cards,
% and gives all the valid sublists as long as it is true
find_card_groups([], []).
find_card_groups([C|Cards], [C|Found]) :-
    find_card_groups(Cards, Found).
find_card_groups([_|Cards], Found) :-
    find_card_groups(Cards, Found).

%----------------------------------------------------------------------------%
% Purpose: count value of 15s in hand cards combined with Startcard
%----------------------------------------------------------------------------%

% add_to_fifteen(Cards, Found) true 
% when Found is a list of cards that sums to 15
add_to_fifteen([], _).
add_to_fifteen(Cards, Found) :-
    find_card_groups(Cards, Valids),
    replace_rank(Valids, Found),
    sum(Found, #=, 15).

% countScore(Cards, S) true when S is the score of 15s 
% when counting cards score
count_15_score([], 0).
count_15_score(Cards, S) :-
    (\+add_to_fifteen(Cards, _) ->
    List = []
    ;bagof(Found, add_to_fifteen(Cards, Found), List)),
    length(List, S1),
    S is S1 * 2. 

%----------------------------------------------------------------------------%
% Purpose: count value of Pairs in hand cards combined with Startcard
%----------------------------------------------------------------------------%

% find_pair(Cards, Pairs) true when Cards has any Pairs
find_pair([], []).
find_pair(Cards, Pairs) :- 
    find_card_groups(Cards, Subcards),
    check_pair(Subcards, Pairs).

% true when the given list is a pair with same rank (does not care about suit)
% false for empty list
check_pair([card(A,_), card(A,_)], [card(A,_), card(A,_)]).

% count_Pair_Score(Cards, S) true when S is the score of Cards in this game
% when counting pairs 
count_Pair_Score([], 0).
count_Pair_Score(Cards, S) :-
    (\+find_pair(Cards, _) ->
    List = []
    ;bagof(Pairs, find_pair(Cards, Pairs), List)),
    length(List, S1),
    S is S1 * 2.

%----------------------------------------------------------------------------%
% Purpose: count value of runs in hand cards combined with Startcard
%----------------------------------------------------------------------------%

% find_run(Cards, Score) true when Score is the number of runs in Cards
% accumulate Pair of double runs
find_run([], 0, 0).
find_run([_], 0, 0).
find_run([A, B|Cards], Score, Pair) :- 
    card_rank_index(A, I1),
    card_rank_index(B, I2),
    find_run([B|Cards], S1, P1),
    (I2 - I1 =:= 1 ->
    Pair is P1,
   (S1 =:= 0 -> 
       Score is S1 + 2
       ;Score is S1 + 1)
    ;I2 =:= I1 ->
    Pair is P1 + 2,
    Score is S1
    ;Score is 0, Pair is 0).

% count_run_score(Cards, S) true when S is the score get from runs of cards 
% with score must be equal or larger than 3(means run of 3 or more)
count_run_score([], 0).
count_run_score(Cards, Score) :-
    find_run(Cards, S1, P),
    (S1 >= 3 ->
        (P \= 0 ->
        Score is S1 * P
        ;Score is S1)
    ;Score is 0).

%----------------------------------------------%
% Purpose: check if the hand cards are a flush
%----------------------------------------------%

% check_flushes(Hand]) true when all the cards in Hand are the same
check_flushes([_]).
check_flushes([card(_, H), card(_, H)|Hand]) :- 
    check_flushes([card(_, H)|Hand]).

check_nob([], _).
check_nob(Hand, card(R, S)) :-
    member(card(jack, S), Hand),
    R \= jack.


% hand_value(Hand, Startcard, Value) true when value is the value of cards 
% counted follow the rule of the cribbage game
hand_value([], _, 0).
hand_value(Hand, Startcard, Value) :-
    append(Hand, [Startcard], Merged),
    card_sort(Merged, Sorted),
    count_15_score(Sorted, V1),
    count_Pair_Score(Sorted, V2),
    count_run_score(Sorted, V3),
    (check_flushes(Hand) ->
    V4 = 4
    ;V4 = 0),
    (check_nob(Hand, Startcard) -> 
    V5 = 1
    ;V5 = 0),
    Value is V1 + V2 + V3 + V4 + V5.

%--------------------------------------------------------------------------%
% Second part purpose: 
% implement choosing the hand card with possibly highest score
%--------------------------------------------------------------------------%

% select_hand(Cards, Hand, Cribcards) true when 
% Hand is the possibly cards that may get highest value,
% Cribcards are the rest that will not be kept
select_hand([], [], _).
select_hand(Cards, Hand, Cribcards) :-
    find_highest_value(Cards, Hand),!,
    subtract(Cards, Hand, Cribcards).

% find_highest_value(Cards, Highesthand) true when Highesthand is the
% hand of cards that gets the highest score on average
find_highest_value(Cards, Highesthand) :-
    findall(Rest, get_each(Cards, Rest), Handlist),
    count_values(Cards, Handlist, Valuelist),
    max_list(Valuelist, Value),
    nth0(I, Valuelist, Value),
    nth0(I, Handlist, Highesthand).
    
% get_each(Cards, Rest) true when Rest is all the posible groups of 4
% generated from Cards
get_each([], [], _).
get_each(Cards, Rest) :-
    find_card_groups(Cards, Found),
    length(Found, 4),
    Rest = Found.

% count_values(Cards, Handlist, Valuelist) true when value is the 
% average value of each hand from Handlist, Cards is for excluding the 
% Cribcards and Hand cards when generating one card as predicted start card
count_values(_, [], []).
count_values(Cards, [H|Handlist], [V|Valuelist]) :-
    avg_value(Cards, H, V),
    count_values(Cards, Handlist, Valuelist).

% get_value(Cards, Hand, Value) true 
% when Value is the value of Hand and a predicted start card
get_value(Cards, Hand, Value) :-
    generate_one_card(Cards, Onecard),
    hand_value(Hand, Onecard, Value).

% avg_value(Cards, Hand, Avgvalue) true when Avgvalue is the average value 
% of Hand combining with all the possible start cards
avg_value(Cards, Hand, Avgvalue) :-
    bagof(Value, get_value(Cards, Hand, Value), Valuelist),
    sum_list(Valuelist, Sum),
    length(Valuelist, L),
    Avgvalue is Sum/L.

% generate_one_card(Cards, Onecard) true when Onecard is a valid card 
% but not an element in the Cards which is the list of Cribcards and Hand cards
generate_one_card(Cards, Onecard) :-
    between(0, 3, N1),
    nth0(N1, [clubs, diamonds, hearts, spades], Rank),
    between(0, 13, N2),
    nth0(N2, [ace,2,3,4,5,6,7,8,9,10,jack,queen,king], Suit),
    \+ member(Cards, card(Suit, Rank)),
    Onecard = card(Suit, Rank).

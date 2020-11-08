# COMP30020-DP-Project1
>The first project of Declarative Programming with Prolog 2020 s2

Introduction
============
>This project aim to implement two functions for a card game called Cribbage.

Implementation
==============
> hand_value(Hand, Startcard, Value)
Value (an integer) is the total cribbage point value of Hand when Startcard is the start card. Hand is represented as a list of 4 card terms, and Startcard is a single card term. A card term is a term card(Rank, Suit), where Rank is either an integer between 2 and 10, or one of ace, jack, queen, or king, and Suit is one of clubs, diamonds, hearts, or spades.

> select_hand(Cards, Hand, Cribcards)
Cards is a list of the 5 or 6 cards dealt to a player at the start of a hand. Hand is a list of 4 of those cards to be kept to form the players hand, and Cribcards is a list of the cards not kept (to be placed in the crib). The cards to be kept in the hand should be chosen to maximize the expected value of the hand over all possible start cards. Since the start card is not known when the hand is chosen, you cannot be sure to choose the best hand. However the expected value of the hand is the average value of the hand over all possible start cards (Cribcards and Hand cards are not possible start cards). This predicate need only work when Cards is a ground list of card terms.

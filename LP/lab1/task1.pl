/*
    task1.pl
        1. Вариант 6. Удаление N последних элементов списка
        2. Вариант 11. Вычисление позиции максимального элемента в списке
        3. Реализовать свои версии стандартных предикатов обработки списков, 
            рассмотренные на занятии (length, member, append, remove, permute, sublist), 
            убедиться в их работоспособности на ряде различных запросов.
*/

:- encoding(utf8).

% 1
% std
remove_last_n_std(List, N, Result) :-
    length(List, Len),
    KeepLen is Len - N,
    length(Result, KeepLen),
    append(Result, _, List).

% rec
remove_last_n_rec(List, N, Result) :-
    length_rec(List, Len),
    KeepLen is Len - N,
    take_n_rec(List, KeepLen, Result).

take_n_rec(_, 0, []) :- !.
take_n_rec([], _, []).
take_n_rec([H|T], N, [H|R]) :-
    N1 is N - 1,
    take_n_rec(T, N1, R).

% 2
% std
max_pos_std(List, Pos) :-
    max_list(List, Max),
    nth1(Pos, List, Max).

% rec
max_pos_rec([H|T], Pos) :-
    max_pos_rec(T, 2, H, 1, Pos).

max_pos_rec([], _, _, MaxPos, MaxPos).
max_pos_rec([H|T], CurPos, CurMax, MaxPos, ResultPos) :-
    ( H > CurMax ->
        NewMax = H,
        NewMaxPos = CurPos
    ;
        NewMax = CurMax,
        NewMaxPos = MaxPos
    ),
    NextPos is CurPos + 1,
    max_pos_rec(T, NextPos, NewMax, NewMaxPos, ResultPos).


% 3
% my
length_rec([], 0).
length_rec([_|T], N) :-
    length_rec(T, N1),
    N is N1 + 1.

member_rec(X, [X|_]).
member_rec(X, [_|T]) :-
    member_rec(X, T).

append_rec([], L, L).
append_rec([H|T], L2, [H|R]) :-
    append_rec(T, L2, R).

permute_rec([], []).
permute_rec(L, [H|T]) :-
    remove_rec(H, L, Rest),
    permute_rec(Rest, T).

remove_rec(X, [X|T], T).
remove_rec(X, [H|T], [H|R]) :-
    remove_rec(X, T, R).

sublist_rec(Sub, List) :-
    append_rec(_, Rest, List),
    append_rec(Sub, _, Rest).
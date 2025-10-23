% task1.pl
% 1. Вариант 6. Удаление N последних элементов
% 2. Вариант 11. Вычисление позиции максимального элемента в списке

remove_last_n_std(List, N, Result) :-
    length(List, Len),
    KeepLen is Len - N,
    length(Result, KeepLen),
    append(Result, _, List).

remove_last_n_rec(List, N, Result) :-
    length_rec(List, Len),
    KeepLen is Len - N,
    take_n_rec(List, KeepLen, Result).

length_rec([], 0).
length_rec([_|T], L) :-
    length_rec(T, L1),
    L is L1 + 1.

take_n_rec(_, 0, []) :- !.
take_n_rec([], _, []).
take_n_rec([H|T], N, [H|R]) :-
    N1 is N - 1,
    take_n_rec(T, N1, R).

max_pos_std(List, Pos) :-
    max_list(List, Max),
    nth1(Pos, List, Max).

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

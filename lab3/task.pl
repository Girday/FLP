% Вариант 6: Задача о перемещении черных и белых шаров

start_state([ч,ч,ч,ч,п,б,б,б]).
goal_state([б,б,б,п,ч,ч,ч,ч]).


% ГЕНЕРАЦИЯ ХОДОВ

find_empty(State, Pos) :-
    nth0(Pos, State, п).

move(State, NewState, Move) :-
    find_empty(State, EmptyPos),
    ( move_black_step(State, EmptyPos, NewState, Move)
    ; move_black_jump(State, EmptyPos, NewState, Move)
    ; move_white_step(State, EmptyPos, NewState, Move)
    ; move_white_jump(State, EmptyPos, NewState, Move)
    ).

move_black_step(State, EmptyPos, NewState, Move) :-
    EmptyPos > 0,
    BallPos is EmptyPos - 1,
    nth0(BallPos, State, ч),
    swap(State, BallPos, EmptyPos, NewState),
    format_move(BallPos, EmptyPos, 'черный шаг вправо', Move).

move_black_jump(State, EmptyPos, NewState, Move) :-
    EmptyPos > 1,
    BallPos is EmptyPos - 2,
    MiddlePos is EmptyPos - 1,
    nth0(BallPos, State, ч),
    nth0(MiddlePos, State, Ball),
    Ball \= п,
    swap(State, BallPos, EmptyPos, NewState),
    format_move(BallPos, EmptyPos, 'черный прыжок вправо', Move).

move_white_step(State, EmptyPos, NewState, Move) :-
    length(State, Len),
    EmptyPos < Len - 1,
    BallPos is EmptyPos + 1,
    nth0(BallPos, State, б),
    swap(State, BallPos, EmptyPos, NewState),
    format_move(BallPos, EmptyPos, 'белый шаг влево', Move).

move_white_jump(State, EmptyPos, NewState, Move) :-
    length(State, Len),
    EmptyPos < Len - 2,
    BallPos is EmptyPos + 2,
    MiddlePos is EmptyPos + 1,
    nth0(BallPos, State, б),
    nth0(MiddlePos, State, Ball),
    Ball \= п,
    swap(State, BallPos, EmptyPos, NewState),
    format_move(BallPos, EmptyPos, 'белый прыжок влево', Move).

swap(List, I, J, Result) :-
    nth0(I, List, ElemI),
    nth0(J, List, ElemJ),
    replace_nth(List, I, ElemJ, Temp),
    replace_nth(Temp, J, ElemI, Result).

replace_nth([_|T], 0, X, [X|T]).
replace_nth([H|T], N, X, [H|R]) :-
    N > 0,
    N1 is N - 1,
    replace_nth(T, N1, X, R).

format_move(From, To, Type, move(From, To, Type)).


% ПОИСК В ГЛУБИНУ (DFS)

solve_dfs(Solution) :-
    start_state(Start),
    goal_state(Goal),
    get_time(T0),
    dfs([Start], Goal, [], RevPath),
    reverse(RevPath, Solution),
    get_time(T1),
    Time is T1 - T0,
    length(Solution, Len),
    format('Решение найдено за ~3f сек~n', [Time]),
    format('Длина пути: ~d ходов~n', [Len]).

dfs([State|_], Goal, Path, [State|Path]) :-
    State = Goal.

dfs([State|Rest], Goal, Visited, Solution) :-
    State \= Goal,
    findall(NextState, 
            (move(State, NextState, _), \+ member(NextState, Visited)),
            NextStates),
    append(NextStates, Rest, NewStates),
    dfs(NewStates, Goal, [State|Visited], Solution).


% ПОИСК В ШИРИНУ (BFS)

solve_bfs(Solution) :-
    start_state(Start),
    goal_state(Goal),
    get_time(T0),
    bfs([[Start]], Goal, [], RevPath),
    reverse(RevPath, Solution),
    get_time(T1),
    Time is T1 - T0,
    length(Solution, Len),
    format('Решение найдено за ~3f сек~n', [Time]),
    format('Длина пути: ~d ходов~n', [Len]).

bfs([[State|Path]|_], Goal, _, [State|Path]) :-
    State = Goal.

bfs([[State|Path]|RestPaths], Goal, Visited, Solution) :-
    State \= Goal,
    findall([NextState, State|Path],
            (move(State, NextState, _), \+ member(NextState, Visited)),
            NewPaths),
    append(RestPaths, NewPaths, UpdatedPaths),
    bfs(UpdatedPaths, Goal, [State|Visited], Solution).


% ИТЕРАТИВНОЕ УГЛУБЛЕНИЕ (IDDFS)

solve_iddfs(Solution) :-
    start_state(Start),
    goal_state(Goal),
    get_time(T0),
    iddfs_loop(Start, Goal, 0, RevPath),
    reverse(RevPath, Solution),
    get_time(T1),
    Time is T1 - T0,
    length(Solution, Len),
    format('Решение найдено за ~3f сек~n', [Time]),
    format('Длина пути: ~d ходов~n', [Len]).

iddfs_loop(Start, Goal, Depth, Path) :-
    dfs_limited_id(Start, Goal, Depth, [Start], Path), !.
iddfs_loop(Start, Goal, Depth, Path) :-
    Depth1 is Depth + 1,
    iddfs_loop(Start, Goal, Depth1, Path).

dfs_limited_id(State, Goal, _Limit, Path, Path) :-
    State = Goal.

dfs_limited_id(State, Goal, Limit, PathSoFar, Path) :-
    Limit > 0,
    Limit1 is Limit - 1,
    findall(NextState,
            (move(State, NextState, _),
             \+ member(NextState, PathSoFar)),
            Children),
    try_children_iddfs(Children, Goal, Limit1, PathSoFar, Path).

try_children_iddfs([Child|_], Goal, Limit, PathSoFar, Path) :-
    dfs_limited_id(Child, Goal, Limit, [Child|PathSoFar], Path), !.
try_children_iddfs([_|Rest], Goal, Limit, PathSoFar, Path) :-
    try_children_iddfs(Rest, Goal, Limit, PathSoFar, Path).

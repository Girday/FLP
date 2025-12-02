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
    format('~n=== ПОИСК В ГЛУБИНУ (DFS) ===~n', []),
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
    format('~n=== ПОИСК В ШИРИНУ (BFS) ===~n', []),
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
    format('~n=== ИТЕРАТИВНОЕ УГЛУБЛЕНИЕ (IDDFS) ===~n', []),
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


% ЭВРИСТИКИ И HEURISTIC-SEARCH

heuristic(State, H) :-
    goal_state(Goal),
    findall(1,
        ( nth0(I, State, X),
          nth0(I, Goal, Y),
          X \= Y,
          X \= п
        ),
        L),
    length(L, H).


% ЭВРИСТИЧЕСКИЙ ПОИСК (A*)

solve_astar(Solution) :-
    start_state(Start),
    goal_state(Goal),
    get_time(T0),
    heuristic(Start, H0),
    astar([node(Start,[Start],0,H0)], Goal, RevPath, NodesExpanded),
    reverse(RevPath, Solution),
    get_time(T1),
    Time is T1 - T0,
    length(Solution, Len),
    format('~n=== ЭВРИСТИЧЕСКИЙ ПОИСК (A*) ===~n', []),
    format('Решение найдено за ~3f сек~n', [Time]),
    format('Длина пути: ~d ходов~n', [Len]),
    format('Расширено узлов: ~d~n', [NodesExpanded]).

astar(Open, Goal, Path, NodesExpanded) :-
    select_best_node(Open, node(State, PathSoFar, G, _F), RestOpen),
    ( State = Goal ->
        Path = PathSoFar, NodesExpanded = 1
    ;
        findall(node(Next, [Next|PathSoFar], G1, F1),
            ( move(State, Next, _),
              \+ member(Next, PathSoFar),
              G1 is G + 1,
              heuristic(Next, H1),
              F1 is G1 + H1
            ),
            Children),
        append(RestOpen, Children, NewOpen),
        astar(NewOpen, Goal, Path, NodesExpandedRest),
        NodesExpanded is NodesExpandedRest + 1
    ).

select_best_node([N], N, []) :- !.
select_best_node([node(S,P,G,F)|T], Best, [Other|Rest]) :-
    select_best_node(T, Best1, Rest1),
    Best1 = node(_,_,G1,F1),
    ( F < F1 -> Best = node(S,P,G,F), Other = Best1, Rest = Rest1
    ; F =:= F1, G =< G1 -> Best = node(S,P,G,F), Other = Best1, Rest = Rest1
    ; Best = Best1, Other = node(S,P,G,F), Rest = Rest1
    ).


% ЖАДНЫЙ ПОИСК (GREEDY BEST-FIRST)

solve_greedy(Solution) :-
    start_state(Start),
    goal_state(_),
    get_time(T0),
    heuristic(Start, H0),
    greedy([gnode(Start,[Start],H0)], RevPath, Nodes),
    reverse(RevPath, Solution),
    get_time(T1),
    Time is T1 - T0,
    length(Solution, Len),
    format('~n=== ЖАДНЫЙ ПОИСК (GREEDY BEST-FIRST) ===~n', []),
    format('Решение найдено за ~3f сек~n', [Time]),
    format('Длина пути: ~d ходов~n', [Len]),
    format('Расширено узлов: ~d~n', [Nodes]).

greedy(Open, Path, NodesExpanded) :-
    select_best_greedy(Open, gnode(State, PathSoFar, _H), Rest),
    ( goal_state(Goal), State = Goal ->
        Path = PathSoFar, NodesExpanded = 1
    ;
        findall(gnode(Next, [Next|PathSoFar], Hn),
            ( move(State, Next, _), \+ member(Next, PathSoFar), heuristic(Next, Hn) ),
            Children),
        append(Rest, Children, NewOpen),
        greedy(NewOpen, Path, NodesRest),
        NodesExpanded is NodesRest + 1
    ).

select_best_greedy([X], X, []) :- !.
select_best_greedy([gnode(S,P,H)|T], Best, [Other|Rest]) :-
    select_best_greedy(T, Best1, Rest1),
    Best1 = gnode(_,_,H1),
    ( H < H1 -> Best = gnode(S,P,H), Other = Best1, Rest = Rest1
    ; Best = Best1, Other = gnode(S,P,H), Rest = Rest1
    ).


% ИТЕРАТИВНОЕ УГЛУБЛЕНИЕ (IDA*)

solve_idastar(Solution) :-
    start_state(Start),
    goal_state(_Goal),
    get_time(T0),
    heuristic(Start, H0),
    MaxBoundLimit = 200,
    ida_loop([Start], 0, H0, MaxBoundLimit, Solution),
    get_time(T1),
    Time is T1 - T0,
    length(Solution, Len),
    format('~n=== ИТЕРАТИВНОЕ УГЛУБЛЕНИЕ (IDA*) ===~n', []),
    format('Решение найдено за ~3f сек~n', [Time]),
    format('Длина пути: ~d ходов~n', [Len]).

ida_loop(StartPath, _G, Bound, MaxBound, Solution) :-
    Bound =< MaxBound,
    ( depth_limited_f(StartPath, 0, Bound, Solution) ->
        true
    ; Bound1 is Bound + 1,
      ida_loop(StartPath, 0, Bound1, MaxBound, Solution)
    ).

depth_limited_f(Path, G, Bound, Solution) :-
    Path = [Node|_],
    heuristic(Node, H),
    F is G + H,
    F =< Bound,
    ( goal_state(Goal), Node = Goal ->
        reverse(Path, Solution)
    ;
        findall(Next, ( move(Node, Next, _), \+ member(Next, Path) ), Nexts),
        G1 is G + 1,
        try_nexts(Nexts, Path, G1, Bound, Solution)
    ).

try_nexts([N|_], Path, G, Bound, Solution) :-
    depth_limited_f([N|Path], G, Bound, Solution), !.
try_nexts([_|T], Path, G, Bound, Solution) :-
    try_nexts(T, Path, G, Bound, Solution).

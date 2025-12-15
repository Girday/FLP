% Вариант 6

true(river_volga).
true(pupil_vasia).
true(sun_hot).
true(water_wet).
true(snow_white).
true(sky_blue).
true(fire_dangerous).
true(math_difficult).
true(prolog_logical).
true(earth_round).
true(monday_weekday).
true(summer_warm).
true(winter_cold).
true(cats_cute).
true(dogs_loyal).


% Главный предикат вычисления
calculate(Expression, Result) :-
    phrase(parse_expression(AST), Expression),
    evaluate(AST, Result).

% Точка входа грамматики
parse_expression(AST) --> parse_implication(AST).

% Следование (=>)
parse_implication(implication(Left, Right)) -->
    parse_disjunction(Left),
    ['=>'],
    parse_implication(Right).
parse_implication(AST) --> parse_disjunction(AST).

% Дизъюнкция (V)
parse_disjunction(AST) -->
    parse_conjunction(Left),
    parse_disjunction_rest(Left, AST).

parse_disjunction_rest(Left, AST) -->
    ['V'],
    parse_conjunction(Right),
    { NewLeft = disjunction(Left, Right) },
    parse_disjunction_rest(NewLeft, AST).
parse_disjunction_rest(AST, AST) --> [].

% Конъюнкция (&)
parse_conjunction(AST) -->
    parse_negation(Left),
    parse_conjunction_rest(Left, AST).

parse_conjunction_rest(Left, AST) -->
    ['&'],
    parse_negation(Right),
    { NewLeft = conjunction(Left, Right) },
    parse_conjunction_rest(NewLeft, AST).
parse_conjunction_rest(AST, AST) --> [].

% Отрицание (~)
parse_negation(negation(AST)) -->
    ['~'],
    parse_negation(AST).
parse_negation(AST) --> parse_primary(AST).

% Первичные выражения
parse_primary(AST) -->
    ['('],
    parse_expression(AST),
    [')'].
parse_primary(const(true)) --> [true].
parse_primary(const(false)) --> [false].
parse_primary(atom(Atom)) --> [Atom], { atom(Atom), Atom \= true, Atom \= false }.

% Вычисление констант
evaluate(const(true), true).
evaluate(const(false), false).

% Вычисление атомов
evaluate(atom(Atom), true) :- true(Atom), !.
evaluate(atom(_), false).

% Вычисление отрицания
evaluate(negation(Expr), Result) :-
    evaluate(Expr, Val),
    negate(Val, Result).

% Вычисление конъюнкции
evaluate(conjunction(Left, Right), Result) :-
    evaluate(Left, LeftVal),
    evaluate(Right, RightVal),
    and(LeftVal, RightVal, Result).

% Вычисление дизъюнкции
evaluate(disjunction(Left, Right), Result) :-
    evaluate(Left, LeftVal),
    evaluate(Right, RightVal),
    or(LeftVal, RightVal, Result).

% Вычисление импликации
evaluate(implication(Left, Right), Result) :-
    evaluate(Left, LeftVal),
    evaluate(Right, RightVal),
    implies(LeftVal, RightVal, Result).

% Логическое НЕ
negate(true, false).
negate(false, true).

% Логическое И
and(true, true, true).
and(_, _, false).

% Логическое ИЛИ
or(false, false, false).
or(_, _, true).

% Логическая импликация
implies(false, _, true).
implies(true, true, true).
implies(true, false, false).

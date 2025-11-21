% Вариант 6: Задача про Чука и Гека

:- encoding(utf8).


% Определение дней, когда каждый из них врёт

lies(chuk, monday).
lies(chuk, tuesday).
lies(chuk, wednesday).

lies(gek, tuesday).
lies(gek, thursday).
lies(gek, saturday).

% Предикат: говорит ли человек правду в данный день
tells_truth(Person, Day) :- not(lies(Person, Day)).


% Предикаты для работы с днями недели

day(monday).
day(tuesday).
day(wednesday).
day(thursday).
day(friday).
day(saturday).
day(sunday).

next_day(sunday, monday).
next_day(monday, tuesday).
next_day(tuesday, wednesday).
next_day(wednesday, thursday).
next_day(thursday, friday).
next_day(friday, saturday).
next_day(saturday, sunday).

prev_day(Today, Yesterday) :- next_day(Yesterday, Today).


% Проверка утверждений персонажей

% Утверждение 1: "Меня зовут Чук"
% Правда, если: человек = Чук И говорит правду
% Ложь, если: человек = Гек И врёт ИЛИ человек = Чук И врёт
says_name_chuk(Person, Day) :-
    Person = chuk,
    tells_truth(Person, Day).
says_name_chuk(Person, Day) :-
    Person = gek,
    lies(Person, Day).

% Утверждение 2: "Вчера было воскресенье" (=> сегодня понедельник)
says_yesterday_sunday(Person, Day) :-
    prev_day(Day, sunday),
    tells_truth(Person, Day).
says_yesterday_sunday(Person, Day) :-
    not(prev_day(Day, sunday)),
    lies(Person, Day).

% Утверждение 3: "Завтра будет пятница" (=> сегодня четверг)
says_tomorrow_friday(Person, Day) :-
    next_day(Day, friday),
    tells_truth(Person, Day).
says_tomorrow_friday(Person, Day) :-
    not(next_day(Day, friday)),
    lies(Person, Day).

% Утверждение 4: "Я всегда говорю правду по средам"
% Правда, если: человек говорит правду по средам И сейчас говорит правду
% Ложь, если: человек врёт по средам И сейчас врёт
says_truth_on_wednesday(Person, Day) :-
    tells_truth(Person, wednesday),
    tells_truth(Person, Day).
says_truth_on_wednesday(Person, Day) :-
    lies(Person, wednesday),
    lies(Person, Day).


% Основной предикат решения задачи

solve(First, Second, Day) :-
    % Перебираем все дни недели
    day(Day),
    
    % Определяем, кто первый, кто второй (они разные)
    member(First, [chuk, gek]),
    member(Second, [chuk, gek]),
    First \= Second,
    
    % Проверяем все утверждения
    says_name_chuk(First, Day),
    says_yesterday_sunday(First, Day),
    says_tomorrow_friday(Second, Day),
    says_truth_on_wednesday(Second, Day).


% Запрос:
% ?- solve(First, Second, Day).

% This code was written with the assistance of generative AI
% Using plunit unit test library included with SWI Prolog
:- use_module(library(plunit)).

:- consult('main.pl').
:- begin_tests(main_tests).

% Test assignClass/2 for expected and edge cases

test(assign_class_first) :- assignClass(100, first).
test(assign_class_first) :- assignClass(75, first).
test(assign_class_first) :- assignClass(70, first).

test(assign_class_upper_second) :- assignClass(69, upperSecond).
test(assign_class_upper_second) :- assignClass(65, upperSecond).
test(assign_class_upper_second) :- assignClass(60, upperSecond).

test(assign_class_lower_second) :- assignClass(59, lowerSecond).
test(assign_class_lower_second) :- assignClass(55, lowerSecond).
test(assign_class_lower_second) :- assignClass(50, lowerSecond).

test(assign_class_fail) :- assignClass(49, third).
test(assign_class_third) :- assignClass(45, third).
test(assign_class_third) :- assignClass(40, third).

test(assign_class_fail) :- assignClass(39, fail).
test(assign_class_fail) :- assignClass(35, fail).
test(assign_class_fail) :- assignClass(0, fail).

% Test student_average_grade/2 by using students in students.pl

% General case
test(student_average_greg) :-
    student_average_grade(greg, Average),
    assertion(Average >= 48),
    assertion(Average =< 49).

% Edge case between 69 and 70
test(student_average_john) :-
    student_average_grade(john, Average),
    assertion(Average >= 69),
    assertion(Average =< 70).

% Test prerequisites_met/2

% Test a student with passing grades to see if they failed a module in the list of prerequisites
test(prerequisites_met_john_webdev) :-
    once(prerequisites_met(john, [introduction_to_programming, introduction_to_computer_science])).

% Test a student with failing grades to see if they failed a module in the list of prerequisites
test(prerequisites_met_unlucky_bob_webdev, fail) :-
    prerequisites_met(unlucky_bob, [introduction_to_programming, introduction_to_computer_science]).

% Test eligible_module/3
% Test a student with passing grades to see if they can take web_development
test(eligible_module_john_webdev) :-
    once(eligible_module(john, web_development, 0.1)).

% Test a student with failing grades to see if they can take web_development
test(eligible_module_unlucky_bob_webdev, fail) :-
    eligible_module(unlucky_bob, web_development, _).

% Test eligible_module_interest/3
% Test a student interested in web development to see if they are eligible
% to take web_development
test(eligible_module_interest_john_webdev) :-
    once(eligible_module_interest(john, web_development, 0.1)).

% Test a student interested in ai to see if they are eligible to take intro_to_ai
test(eligible_module_interest_ai_lover_steve_intro_to_ai) :-
    once(eligible_module_interest(ai_lover_steve, intro_to_ai, 0.2)).

% Test a failing student to see if they are eligible to take more modules
test(eligible_module_interest_unlucky_bob, fail) :-
    eligible_module_interest(unlucky_bob, _, _).

% Test recommend_next/2

% Test if a student with passing grades interested in web development is recommended it.
test(recommend_next_john) :-
    recommend_next(john, web_development).

% Test if a student with passing grades interested in AI is recommended an AI module.
test(recommend_next_ai_lover_steve) :-
    recommend_next(ai_lover_steve, intro_to_ai).

% Test if a student with failing grades is recommended a module.
test(recommend_next_unlucky_bob, fail) :-
    recommend_next(unlucky_bob, _).

:- end_tests(main_tests).
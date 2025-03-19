assignClass(Mark, first)       :- Mark>=70.
assignClass(Mark, upperSecond) :- Mark<70, Mark>=60.
assignClass(Mark, lowerSecond) :- Mark<60, Mark>=50.
assignClass(Mark, third)       :- Mark<50, Mark>=40.
assignClass(Mark, fail)        :- Mark<40.

:- consult('modules.pl').
:- consult('students.pl').

prerequisites_met(_, []).
prerequisites_met(Student, [Prereq|Rest]) :-
  grade(Student, Prereq, Grade),
  assignClass(Grade, Class),
  Class \= fail,
  prerequisites_met(Student, Rest).

recommend_next(Student, ModuleName) :-
    module(ModuleName, _, Prerequisites, _),
    prerequisites_met(Student, Prerequisites),
    \+ grade(Student, ModuleName, _).  % Ensure the student has not taken it yet.


:- dynamic grade/3.
:- dynamic interest/2.
add_student :-
    write('Enter student name (e.g., john): '),
    read(Student),
    write('Enter module name (e.g., introduction_to_programming): '),
    read(Module),
    write('Enter grade (e.g., 75): '),
    read(Grade),
    assertz(grade(Student, Module, Grade)),
    write('Grade added for '), write(Student), 
    write(' in '), write(Module), 
    write(' with grade '), write(Grade), nl,
    
    write('Enter an interest for the student (e.g., webdev): '),
    read(Interest),
    assertz(interest(Student, Interest)),
    nl,
    write('Interest '), write(Interest),
    write(' added for student'), write(Student), 
    nl.

%print_recommendations([]) :- write("No suitable modules"), nl.


assignClass(Mark, first)       :- Mark>=70.
assignClass(Mark, upperSecond) :- Mark<70, Mark>=60.
assignClass(Mark, lowerSecond) :- Mark<60, Mark>=50.
assignClass(Mark, third)       :- Mark<50, Mark>=40.
assignClass(Mark, fail)        :- Mark<40.

difficulty_range_for_class(first, 0.7, 1.0).
difficulty_range_for_class(upperSecond, 0.5, 0.7).
difficulty_range_for_class(lowerSecond, 0.3, 0.5).
difficulty_range_for_class(third, 0.1, 0.3).
difficulty_range_for_class(fail, 0.0, 0.1).

:- consult('modules.pl').
:- consult('students.pl').

student_average_grade(Student, Average) :-
    findall(Grade, grade(Student, _, Grade), Grades),
    Grades \= [],
    sum_list(Grades, Sum),
    length(Grades, Count),
    Average is Sum / Count.

prerequisites_met(_, []).
prerequisites_met(Student, [Prereq|Rest]) :-
  grade(Student, Prereq, Grade),
  assignClass(Grade, Class),
  Class \= fail,
  prerequisites_met(Student, Rest).

eligible_module(Student, Module, Difficulty) :-
    module(Module, Difficulty, Prerequisites, _),
    prerequisites_met(Student, Prerequisites),
    \+ grade(Student, Module, _),  % Ensure the student has not taken it yet.
    student_average_grade(Student, Avg),
    assignClass(Avg, Class),
    difficulty_range_for_class(Class, _MinDiff, MaxDiff),
    Difficulty =< MaxDiff.

recommend_next(Student, RecommendedModule) :-
    aggregate(max(Difficulty, Module),
              eligible_module(Student, Module, Difficulty),
              max(_MaxDifficulty, RecommendedModule)).


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


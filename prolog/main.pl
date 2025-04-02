% This code was written with the assistance of generative AI

% Assigns a degree class to the grade, using a green cut to
% reduce computation
assignClass(Mark, first)       :- Mark >= 70, !.
assignClass(Mark, upperSecond) :- Mark >= 60, !.
assignClass(Mark, lowerSecond) :- Mark >= 50, !.
assignClass(Mark, third)       :- Mark >= 40, !.
assignClass(_, fail).

% Assigns a difficulty range from 0.0-1.0 for each degree class
difficulty_range_for_class(first, 0.7, 1.0).
difficulty_range_for_class(upperSecond, 0.5, 0.7).
difficulty_range_for_class(lowerSecond, 0.3, 0.5).
difficulty_range_for_class(third, 0.1, 0.3).
difficulty_range_for_class(fail, 0.0, 0.1).

% Looks at modules.pl and students.pl, where student and module facts are stored
:- consult('modules.pl').
:- consult('students.pl').

% Rule to calculate average grade of all modules a student has taken.
% Uses findall to get each grade, findall predicate takes the term to
% be found as the first argument, the condition to find the first argument
% in as the second argument, and the list to collect the result in for the
% third argument
% Grades cannot be empty or it fails
% Finds the sum of the grades, the number of grades, then uses this 
% to calculate the average.
student_average_grade(Student, Average) :-
    findall(Grade, grade(Student, _, Grade), Grades),
    Grades \= [],
    sum_list(Grades, Sum),
    length(Grades, Count),
    Average is Sum / Count.

% Checks if a student meets the prerequisites to complete a module.
% Uses recursion on the list of prerequisites for the module.
% Base case:
% If there are no prerequisites or all prerequisites have been checked, 
% (In [Prereq|Rest] Rest = []) the student can take the module.
% Otherwise, get grade based on student and the prerequisite module 
% being checked.
% Find out the degree class they got
% If they did not fail, apply the next prerequisite module to prerequisites_met.
prerequisites_met(_, []).
prerequisites_met(Student, [Prereq|Rest]) :-
  grade(Student, Prereq, Grade),
  assignClass(Grade, Class),
  Class \= fail,
  prerequisites_met(Student, Rest).

% Checks if the student is eligible for the module without taking into
% account their interest in the subject.
% Gets the module and its difficulty and prerequisites.
% Checks the prerequisites are met and the student has not taken the
% module already.
% Gets the average grade of the student.
% Uses this to get the average degree class for the student.
% Checks the difficulty range for their degree class.
% Difficulty has to be below the max difficulty 
eligible_module(Student, Module, Difficulty) :-
    module(Module, Difficulty, Prerequisites, _),
    prerequisites_met(Student, Prerequisites),
    \+ grade(Student, Module, _),  % Ensure the student has not taken it yet.
    student_average_grade(Student, Avg),
    assignClass(Avg, Class),
    difficulty_range_for_class(Class, _MinDiff, MaxDiff),
    Difficulty =< MaxDiff.

% Checks student eligibility for a module, but also requires the module
% to match the interests of the student.
% Uses field (field as in field of interest) unlike eligible_module/3
% and requires the module to match it.
eligible_module_interest(Student, Module, Difficulty) :-
    module(Module, Difficulty, Prerequisites, Field),
    interest(Student, Field),
    prerequisites_met(Student, Prerequisites),
    \+ grade(Student, Module, _),
    student_average_grade(Student, Avg),
    assignClass(Avg, Class),
    difficulty_range_for_class(Class, _MinDiff, MaxDiff),
    Difficulty =< MaxDiff.

% Main function to recommend the next module a student should take.
% Finds the module with the max difficulty using aggregate(max) that
% also satisfies a predicate for eligibility, and recommends it.
% Tries to find a module that matches the field of interest of the 
% student first, then if no solution is found, do not take field of
% interest into account. Uses a conditional to achieve this.
recommend_next(Student, RecommendedModule) :-
    ( aggregate(max(Difficulty, Module),
                eligible_module_interest(Student, Module, Difficulty),
                max(_MaxDifficulty, RecommendedModule))
    -> true
    ;  aggregate(max(Difficulty, Module),
                 eligible_module(Student, Module, Difficulty),
                 max(_MaxDifficulty, RecommendedModule))
    ).

% Declare as dynamic predicate to allow facts to be added during execution
:- dynamic grade/3.
:- dynamic interest/2.

% Adds modules for a student
% Asks for a module name, reads it as a string, convert type to atom to be added
% to grade3/ with assertz. Asks for grade for the module and converts type to number
% to be added to grade/3 with assertz.
% Repeats using conditional until user types done to add multiple modules.
add_modules(Student) :-
    write('Enter module name (or type done to finish): '),
    read_string(user_input, "\n", "", _, ModuleString),
    atom_string(Module, ModuleString),
    ( Module == done ->
         true
    ;
         write('Enter grade for module '), write(Module), write(': '),
         read_string(user_input, "\n", "", _, GradeString),
         ( number_string(Grade, GradeString) -> true ; Grade = GradeString ),
         assertz(grade(Student, Module, Grade)),
         nl,
         add_modules(Student)
    ).

% Asks user to input a student using add_modules, then to input their field of interest.
% Interest converted to atom then added to interest/2. 
add_student :-
    write('Enter student name (e.g., John): '),
    read_string(user_input, "\n", "", _, StudentString),
    atom_string(Student, StudentString),
    nl,
    add_modules(Student),
    write('Enter an interest for the student (e.g., webdev): '),
    read_string(user_input, "\n", "", _, InterestString),
    atom_string(Interest, InterestString),
    assertz(interest(Student, Interest)),
    nl,
    write('Student '), write(Student),
    write(' added with interest '), write(Interest), nl.

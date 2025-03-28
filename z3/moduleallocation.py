# This code was written with the assistance of generative AI
import random
from z3 import *

def generate_random_expertise_table(nlecturers, nmodules):
    """
    Generate a table of expertise with values between 0 and 5.
    Each row corresponds to a lecturer, and each column to a module.
    """
    return [[random.randint(0, 5) for _ in range(nmodules)] for _ in range(nlecturers)]

def allocate_modules_z3(expertise, M):
    """
    Uses Z3 to allocate modules to lecturers with these constraints:
      Every module is taught by exactly one lecturer.
      No lecturer teaches more than M modules.
      Only assign modules when a lecturer's expertise is greater than 1.
    Z3 maximizes the total expertise.
    
    Returns:
       assignment: A 2D list of 0/1 values where assignment[i][j]==1 means lecturer i teaches module j.
       total_expertise: The sum of expertise values for the assignments.
       If no solution exists, returns (None, None).
    """
    # Number of lecturers and modules (rows and columns)
    nlecturers = len(expertise)
    nmodules = len(expertise[0])
    
    # Create Optimizer opt. An Optimizer is like a solver but can be configured to find an optimal solution.
    opt = Optimize()

    # Create decision variables: x[i][j] is 1 if lecturer i teaches module j, else 0.
    # Declared as Int because it is later summed.
    x = [[Int(f"x_{i}_{j}") for j in range(nmodules)] for i in range(nlecturers)]
    
    # For each lecturer in each module
    for i in range(nlecturers):
        for j in range(nmodules):
            # Add constraints for each decision variable: x[i][j] is either 0 or 1
            opt.add(Or(x[i][j] == 0, x[i][j] == 1))
            # If a lecturer's expertise for a module is <= 1, then they cannot teach that module.
            opt.add(Implies(expertise[i][j] <= 1, x[i][j] == 0))
    
    # Each module must be taught by exactly one lecturer.
    for j in range(nmodules):
        opt.add(Sum([x[i][j] for i in range(nlecturers)]) == 1)
    
    # No lecturer teaches more than M modules.
    for i in range(nlecturers):
        opt.add(Sum([x[i][j] for j in range(nmodules)]) <= M)
    
    # Maximize total expertise of allocated modules.
    total_expertise = Sum([expertise[i][j] * x[i][j] for i in range(nlecturers) for j in range(nmodules)])
    opt.maximize(total_expertise)
    
    if opt.check() == sat:
      # Get optimal model
        model = opt.model()
        # Get 2d matrix for lecturer assignment
        assignment = [[model.evaluate(x[i][j]).as_long() for j in range(nmodules)]
                      for i in range(nlecturers)]
        return assignment, model.evaluate(total_expertise).as_long()
    else:
        return None, None

def print_expertise_table(expertise):
    """Prints the expertise table with module labels."""
    nlecturers = len(expertise)
    nmodules = len(expertise[0])
    header = "       " + " ".join([f"m{j+1:2}" for j in range(nmodules)])
    print(header)
    for i in range(nlecturers):
        row = f"l{i+1:2}:  " + " ".join([f"{expertise[i][j]:2}" for j in range(nmodules)])
        print(row)

def print_assignment(assignment):
    """Prints the lecturer assigned to each module."""
    nlecturers = len(assignment)
    nmodules = len(assignment[0])
    print("\nModule Assignments:")
    for j in range(nmodules):
        for i in range(nlecturers):
            if assignment[i][j] == 1:
                print(f"  Module m{j+1:2} is taught by Lecturer l{i+1:2}")
                break

if __name__ == "__main__":
    random.seed(42)  # For reproducibility
    nlecturers = 6
    nmodules = 20
    M = 4  # Maximum modules per lecturer

    # Generate a random expertise table and display it.
    expertise = generate_random_expertise_table(nlecturers, nmodules)
    print("Expertise Table:")
    print_expertise_table(expertise)

    # Compute the optimal allocation using Z3.
    assignment, total_expertise = allocate_modules_z3(expertise, M)
    
    if assignment is not None:
        print_assignment(assignment)
        print(f"\nTotal Expertise Used: {total_expertise}")
    else:
        print("No feasible solution exists under the given constraints.")
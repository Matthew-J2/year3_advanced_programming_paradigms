# This code was written with the assistance of generative AI
import unittest
from z3 import *
import random
import moduleallocation

class TestZ3Allocation(unittest.TestCase):

    def setUp(self):
        # Create a small, deterministic expertise table for testing.
        # In this table, a value <= 1 means the lecturer should not be assigned the module.
        self.expertise = [
            [3, 2, 4],
            [5, 1, 2],
            [0, 4, 3]
        ]
        self.M = 2  # Maximum modules per lecturer
        self.nlecturers = len(self.expertise)
        self.nmodules = len(self.expertise[0])

    def test_assignment_validity(self):
        assignment, total_expertise = moduleallocation.allocate_modules_z3(self.expertise, self.M)
        self.assertIsNotNone(assignment, "Assignment should not be None if a solution exists")
        
        # Check that each module is assigned to exactly one lecturer.
        for j in range(self.nmodules):
            module_sum = sum(assignment[i][j] for i in range(self.nlecturers))
            self.assertEqual(module_sum, 1, f"Module {j} is assigned to {module_sum} lecturers (should be exactly one)")
        
        # Check that no lecturer is assigned more than M modules.
        for i in range(self.nlecturers):
            lecturer_sum = sum(assignment[i][j] for j in range(self.nmodules))
            self.assertLessEqual(lecturer_sum, self.M, f"Lecturer {i} teaches {lecturer_sum} modules (should be <= {self.M})")
        
        # Verify that a lecturer with expertise <= 1 is not assigned that module.
        for i in range(self.nlecturers):
            for j in range(self.nmodules):
                if self.expertise[i][j] <= 1:
                    self.assertEqual(assignment[i][j], 0,
                                     f"Lecturer {i} should not be assigned module {j} with insufficient expertise {self.expertise[i][j]}")

    def test_total_expertise_calculation(self):
        assignment, total_expertise = moduleallocation.allocate_modules_z3(self.expertise, self.M)
        # Calculate the total expertise based on the assignment.
        calculated_expertise = sum(self.expertise[i][j] * assignment[i][j]
                                   for i in range(self.nlecturers)
                                   for j in range(self.nmodules))
        self.assertEqual(total_expertise, calculated_expertise,
                         "Total expertise calculated by the model does not match the computed total.")

if __name__ == '__main__':
    unittest.main()
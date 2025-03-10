from z3 import *

def main():
  solver = Solver()
  words = ("aster", "derbyshire", "cheers", "sonder", "brain", "decadent", "correspondant", "xylophone", "information", "beneficiary", "firefighter", "rhythm", "elephant", "flag", "baseball", "appoint", "sculpture", "terrace", "vampire", "love")
  z3_words = Array("strings", IntSort(), StringSort())
  #z3_grid = 
  for idx, val in enumerate(words):
    # Add rules here
    # Rules for a crossword problem
    # For at least 10 (at random) words:
    # Each word must have a letter in common 
    # Crossword has a number of rows and columns, let's say 15x15
    # So words must fit within this
    # Word length must match the spaces allocated to it
    

  if solver.check == sat:
    model = solver.model()
  else:
    print("No crossword for you")

if __name__ == "__main__":
  main()
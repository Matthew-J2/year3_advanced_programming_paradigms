from z3 import *

def main():
  solver = Solver()
  words = ("aster", "derbyshire", "cheers", "sonder", "brain", "decadent", "correspondant", "xylophone", "information", "beneficiary", "firefighter", "rhythm", "elephant", "flag", "baseball", "appoint", "sculpture", "terrace", "vampire", "love")
  z3_words = Array("strings", IntSort(), StringSort())
  rows, cols, = 15, 15
  grid = [[0]*cols]*rows
  z3_grid = [[String(f"Row {j}, Col {i}") for i in range (cols)] for j in range (rows)]
  print(z3_grid, "\n\n")

  steamed = "steamed"
  hams = "hams"
  for i in range(len(steamed)):
    print(i, steamed[i])
    solver.add(z3_grid[1][i] == StringVal(steamed[i]))
  
  for i in range(len(hams)):
    print(i, hams[i])
    solver.add(z3_grid[i][3] == StringVal(hams[i]))
  
  solver.add(z3_grid[1][3] == StringVal("a"))
  #for idx, val in enumerate(words):
    # Add rules here
    # Rules for a crossword problem
    # For at least 10 (at random) words:
    # Each word must have a letter in common 
    # Crossword has a number of rows and columns, let's say 15x15
    # So words must fit within this
    # Word length must match the spaces allocated to it
    
  

  if solver.check() == sat:
    model = solver.model()
    for i in range(rows):
      print([model[z3_grid[i][j]] for j in range(cols)])
  else:
    print("No crossword for you")

if __name__ == "__main__":
  main()
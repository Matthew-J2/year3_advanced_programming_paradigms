from z3 import *
import random

#TODO: change this to constrain to a correct position instead of trying difdferent ones
def place_word(solver, z3_grid, word, rows, cols, trials=10000):
  word_len = len(word)
  for i in range(trials):
    # Choose a random candidate placement
    cand_row = random.randint(0, rows - 1)
    cand_col = random.randint(0, cols - 1)
    cand_dir = random.choice([0, 1])  # 0 = horizontal 1 = vertical

    # Check if the word would fit in the grid with this candidate position
    if cand_dir == 0 and cand_col + word_len > cols:
      continue
    if cand_dir == 1 and cand_row + word_len > rows:
      continue

    # Saves state (onto stack)
    solver.push()
    # Try the candidate
    for idx, char in enumerate(word):
      if cand_dir == 0:
        r = cand_row
        c = cand_col + idx
      else:
        r = cand_row + idx
        c = cand_col
      # Constrain the candidate cell to equal the character required
      solver.add(Select(z3_grid, r * cols + c) == StringVal(char))
    if solver.check() == sat:
      return cand_row, cand_col, "H" if cand_dir == 0 else "V"
    else:
      # Reverts to last saved state (from stack)
      solver.pop()  # candidate failed; try another
  return None

def main():
  solver = Solver()
  # Bank of words
  words = ("aster", "derbyshire", "cheers", "sonder", "brain", "decadent", "correspondant", "xylophone", "information", "beneficiary", "firefighter", "rhythm", "elephant", "flag", "baseball", "appoint", "sculpture", "terrace", "vampire", "love", "cheese", "tomatoes", "ketchup", "gravy", "pies", "potato", "spaghetti", "adam", "bicycle", "iphone", "theta", "cat", "dog", "greg", "bin", "man", "ham", "gig", "wig")
  #words = ("aster", "derbyshire", "cheers", "sonder", "brain", "decadent", "correspondant", "xylophone", "information", "beneficiary", "firefighter", "rhythm", "elephant", "flag")
  rows, cols = 15, 15
  z3_grid = Array("z3_grid", IntSort(), StringSort())

  # Try to place each word in the grid
  for word in words:
    result = place_word(solver, z3_grid, word, rows, cols)
    if result:
      print(f"Placed {word} at {result[0]}, {result[1]}, {result[2]}")
    else:
      print(f"No {word} allowed")
  
  # If sat, print output
  if solver.check() == sat:
    model = solver.model()
    for r in range(rows):
      row_output = []
      for c in range(cols):
        index = r * cols + c
        # TODO: fix the grid printing e as a blank space
        value = model.evaluate(Select(z3_grid, index), model_completion=True)
        char = value.as_string() if value.as_string() != "" else "."
        row_output.append(char)
      print(" ".join(row_output))
  else:
    print("No crossword for you")

if __name__ == "__main__":
    main()

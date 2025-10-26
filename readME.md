Project:
  Name: CS4337 Project 1 - Prefix Calculator
  Description: A prefix-notation calculator implemented in Racket that supports
    interactive and batch modes. It maintains a history of results and
    handles invalid expressions.

Files:
  - calculator.rkt --> Main program that evaluates prefix expressions.
  - mode.rkt --> Helper file from professor that defines prompt? (interactive/batch mode).
  - devlog.md --> Development log with dated entries for each coding session.
  - input.txt --> Sample batch test file containing multiple prefix expressions.
  - README.md --> Instructions for running, testing, and general info of the project

Rules:
  - operators:
    "+": Addition
    "*": Multiplication
    "/": Integer division (Error if dividing by zero)
    "-": Unary negation only (not subtraction)
  - history: $n refers to the nth previous result.
  - quit_command: Type 'quit' to exit.
  - error handling: Prints 'Error: Invalid Expression' for malformed or invalid inputs.
  - **spacing requirements: Spaces are required between all numbers, operators, and subexpressions.
    The only exception is history references ($n), which can appear directly next to
    numbers or operators (e.g., + * 2 $1 + $2 1).

Run Instructions:
  interactive_mode:
    command: "racket calculator.rkt"
    behavior:
     - Displays a prompt (>) for user input.
     - Type prefix expressions followed by Enter.
     - Type 'quit' to exit.
    example input:
      - "+ 2 3"
      - "quit"
    example output:
      - "1: 5.0"
  batch_mode:
    command: "racket calculator.rkt -b < input.txt"
    behavior: 
      - Runs the calculator in batch mode, reading expressions from a file.
      - No prompts are displayed; only results and errors print.
    example input.txt: 
      + 2 3
      * + 2 3 4
      / $1 2
      quit
    example output: 
      1: 5.0
      2: 20.0
      3: 2.0

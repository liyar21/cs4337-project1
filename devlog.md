#Devlog
#10-24-25 5:00pm

Starting Project 1
Goal: Build prefix calculator in Racket
Plan: set up files, test interactive and batch mode


##10-24-25 5:55pm
Plan: add mode.rkt and calculator.rkt, run tests

##10-25-25 12:32 am
- Uploaded my first trial of code
- Error occured because mode.rkt was missing the #lang racket declaration at the top

##10-25-25 1:10 am
- Early working draft of calculator.rkt; still has error and needs fix in multiple areas

##10-25-25 1:51 am
- Starting over from the scratchGoal: Set up a simple calculator base.
- Changes Made: Added loop to show prompt and read input, Echoes user input, Handles quit and EOF.
- Issues/Notes:No math or parsing yet.

##10-25-25 11:30 am
- Added function `eval-line` to handle space-separated inputs.
- Added `+` operation for two numbers.

##10-25-25 12:10 pm
- Added -, *, / operators.
- Implemented divide-by-zero check.

##10-25-25 12:40 pm
- Added tokens to remove empty spaces.
- Modified parsing to accept negative values.

##10-25-25 1:10 pm
- Updated number parsing to accept negative values.
- Simplified REPL structure for easier testing.

##10-25-25 1:23pm
- Realized that $1 will recall the oldest result instead of the most recent - forgot to reverse history
- Didn't update history list each loop
- The calculator won't remember new results across lines.

##10-25-25 2:27pm
- Changes Made:
  - Added history tracking to `repl`.
  - Implemented `$n` syntax for recalling results.
  - History now updates with each new calculation.

- History is now passed as a parameter and updated using (cons val hist).
- Each result is displayed with an ID format like "id: value".
- Added floating-point conversion with real->double-flonum.

- failed parsing for the lines with $

##10-25-25 4:10pm
- fixed batch input parsing and $n history handling

##10-25-25 4:29pm
- realized input limited to 3 tokens; planning multi-token fix
- Tested cases like '+ * 2 3 4' and '+ $1 2 3' - program returned "invalid format".

##10-25-25 5:15pm
- Recursive parser that reads prefix left-to-right.
- Added unary negation and integer division.
- Got $n history recall and nested ops working.
- Kept both modes running fine with solid error handling.

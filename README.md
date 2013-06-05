ia_scheduling
=============

Project assignment for the Artificial Intelligence course @ IST 2012

Simplified Scheduling problem in Common LISP.

Problem:
You must satisfy all tasks for a given list of tasks.  
There are several alternatives to satisfy a task - at least one must be satisfied.  
Each alternative is a set of subtasks.  
Different subtasks cannot be done at the same time.  

Subtasks (activities) have a duration of 1 hour and are defined by:
 - Day, a positive integer
 - Hour, an integer between 0 and 23 (inclusive)
 - ID, an identifier.

Tasks with the same identifier are assumed to be the same (if you do one of them, you do not need to do it again).  
Alternatives are a set of subtasks.  
Tasks are a set of alternatives.  

Formulations:
- Formulation A: the most efficient. Has a tree structure.
- Formulation B: Has a graph structure (repeats states).

Heuristics (for Formulation A):
- A1: dumb "return 0" heuristic. (admissible and consistent)
- A2: dumb greedy heuristic.
- A3: not so dumb greedy heuristic - choose task with alternative that imposes fewer subtasks (removing duplicates).
- A4: improvement over A3 - considers all the task's alternatives
- A5: improvement over A4 - considers all tasks (admissible and consistent)
- A6: improvement over A5 - also detects paths leading to dead-ends (saves some processing time)

Search algorithms (tree and graph versions):
- PPP: Depth First Search
- PPPL: Limited Depth First Search
- PPPI: Iterative Deepening First Search
- PLP: Breadth First Search
- PCU: Uniform Cost Search
- PA*: A* Search
- RBFS: Recursive Best-First Search

- CSP: Constraint Satisfaction Problem:
   - Minimum Degree Heuristic
   - Most Restricted Value Heuristic
   - TODO: Minimum Remaining Values Heuristic
   - TODO: MAC (Maintaining Arc Consistency) algorithm

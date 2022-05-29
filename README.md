**HW 2: Due June 2, 2022, 23:59**

Verification for Security - Assignment 2 (10 pts)
=======

In this assignment, you will implement a verifier with Constrained Horn Clauses.
The program flow consists of 4 steps.

    (1) Parsing ECMAScript to Nano (see `Nano.hs`)
    (2) Generation of Horn Clauses from Nano using VCGen (see `HornVCs.hs`)
    (3) Normalizing the Horn Clauses (see `Clauses.hs`)
    (4) Computing a solution for the Horn Clauses using Predicate Abstraction (see `Fixpoint.hs`)

You will implement parts (2-4).

Install
------------
The requirements are the same as for the previous assignment.

As before, you can clone the repository:

    git clone https://github.com/gleissen/hw2.git

and you're good to go.

Running Tests
---
Building and running the verifier is the same as in the previous assignment:

    $ stack run -- tests/pos/while5.js

As before you can run, say, all positive tests using:

    $ stack run -- tests/pos/* 

and all negative tests with 

    $ stack run -- tests/neg/* 

The functions you need to fill in will throw an error: "TODO: FILL THIS IN".

Assignment
----------

This assignment will consist of 3 parts.

## Part 1: Horn Clause generation (2.5 pts)

Provide an implementation for the generation of Horn Clauses from Nano
statements. We will follow the method discussed in class, using weakest
preconditions and VCGen. This will require implementing the following functions
from **HornVCs.hs**:

    - subst          (0.5 pts)
    - generateStmtVC (2 pts)

Function `subst` substitutes an expression for a variable in a query. You will
need this function to implement `generateStmtVC` which translate a Nano program
into Horn clauses.

Horn clauses are defined in file `Clauses.hs`. There are two definitions, `Horn`
for clauses whose head is a query, and `Bound` for clauses whose head is a
formula. You will need to use both types of clauses in your solution.

**Tip:** For a clause `h`, you can use `hd h` to access the head, `bd h` to
access the body, etc.

As before, the Horn clauses created by your method will be kept in the
background state given by monad `VCM`.

You can use functions `addVC` to add regular Horn clauses to the state, and
`addBound` to add bounds.

**Tip:** When defining substitution on queries, you can use the implementation
for substitution on expressions in `Logic.subst_exp`.

**Tip:** In function `generateStmtVC`, you will need to generate fresh queries.
You can use function `freshQuery` for this.

**Tip:** While loops are now annotated with predicates instead of loop
invariants. You will need to thread these predicates through to the solving
algorithm, where they are needed. You can do this by adding them to the `annot`
field in Horn clause definition. We've left a comment in the file to show how
this is done.

**Tip:** There is a test function in `HornVCs.hs` which you can use to test your
implementation. Just uncomment `HornVCs.test` in `Main.hs`.

## Normalization (1 pts)

Our translation to Horn clauses (as discussed in class) produces clauses that
don't fit our syntactic restrictions. That is, queries may contain expressions
rather than just variables. Your second task is to normalize the clauses, such
that they fit our restrictions.

For this, you need to implement the following functions from **Clauses.hs**:

    - normalizeQuery   (0.5 pts)
    - normalizeHorn    (0.25 pts)
    - normalizeBoundM  (0.25 pts)

You can use `nxVar` to create a fresh variable name. Function `normalizeVar`
takes an expression `e` and returns a pair of (1) a variable that will be
used instead of `e`, and (2) a formula that defines the new variable.

You can use this function to define `normalizeQuery` which takes an unnormalized
query (i.e., one that may contain expressions instead of just variables), and
returns a pair of (1) normalized Query and (2) additional definitions.

**Tip:** Take a look at functions `mapM`, `liftM`, `unzip`, `concat`, `fst` and
`snd` which may be useful. Note, you don't have to use these function in your
implementation, if you prefer writing it in a different way.

You can then use `normalizeQuery` to define `normalizeHorn` and
`normalizeBoundM` which normalize regular Horn clauses and bounds, respectively.

## Part 2: Solving Horn Clauses via Predicate Abstraction (4 pts)

Now we have transformed programs into normalized Horn clauses. Your next task is
to implement the solving algorithm, as discussed in class. This will
require implementing the following functions from **Fixpoint.hs**:

    - post            (0.5 pts)
    - pred_abs       (0.5 pts)
    - fixpoint_step   (3 pts)

Your first task is to implement `post`, as discussed in class, however, here
`post`'s first argument are the variables that we want to eliminate.
You will notice that, in class, we used existential quantifiers to eliminate
variables. Here, our logic does not contain existential quantifiers. We can work
around this by substituting the existentially quantified variables by *fresh*
variables. You can use function `freshVars k` as defined in `Monad.hs` to get a
list of `k` fresh variables.

**TIP:** You can use function `substVars` in `Clauses.hs` which substitutes a
list of varibles for another list of varibles.

Function `pred_abs` implements predicate abstraction. That is, it implements
function alpha from the slides. 

**TIP:** You can use function `implies p q` to check whether a formula `p`
implies a formula `q`.

Your main task will be to implement function `fixpoint_step`, which forms the
heart of the solving algorithm. `fixpoint_step` takes as input a list of
predicates `preds`, the current solution `sol`, a work-item `w`, a Horn clause
`h`, and returns a set of new work-items.

The work-item `w` contains a newly computed state (`state w`) for query (`query w`). 
We now want to compute the abstract postcondition with respect to clause
`h`. For this, you have to implement the following steps:

(1) Compute the concrete postcondition `post`. For this, replace any queries in
`h`'s body by their solution, except for query the query (`query w`) from the
work item, which you replace by `state w`. Then, use function `post` to compute
the post-condition. The data-structure for solutions is defined in `Clauses.hs`.

**TIP:** Take a look at functions `plugin` and `get_vars` from `Clauses.hs`.

**TIP:** Solutions are implemented as maps; take look at functions
`Map.lookup`, `Map.adjust` and `const` for working with them.

(2) Next, compute the abstract postcondition, using function `pred_abs`. 

**TIP:** The predicates might be using different variables than the
postcondition you computed in the last step. Make sure that you instantiate them
to the correct variables. Else, none of the predicates will be implied by the
concrete post.

(3) Now you need to check if the new state computed by abstract post is already
a part of the solution. That is, you need to implement the subsumption check.

**NOTE:** When checking subsumption, make sure that the current solution and the
abstract post are instantiated to the same variables (that is, the varibles used
in the head of the current clause). 

**TIP:** You can again use function `implies` for the subsumption check.

(4) If the new state is already contained in the current solution, you can return the empty set.
Else, return a new work-item containing the newly computed state.

**NOTE:** Make sure that your new work item is expressed over the variable used in the solution for its query.

**NOTE:**  As before, you can **only** modify/extend the code in 
the functions mentioned above; there is no need to 
change any code elsewhere.


## Part 3: Verification (2 pts)

Your last task is to verify a small suite of NanoJS programs. As before, you
need to provide annotations to the tests such that your verifier can solve them.
But now, the annotations are predicates. You can provide these with the
following statement:

    pred(P);

Only the positive tests require annotations and as with the previous 
assignment, positive tests should give you `Verification: passed`; 
negative tests should show `Verification: failed`.
See `while5.js` for an example.

**NOTE:** You can **only** write annotations of the form 

    pred(P)

That is, you **cannot** add, remove or modify **any** other lines. 
(Of course, when **debugging** your specifications, you can make 
whatever modifications you like; we just require that the **final** 
versions adhere to the above requirement.) 

## Did it work? (0.5 pts)

Last, is it easier to verify programs in this semi-automated way? Sometimes? For
larger programs? Does getting rid of disjunctions help? For 0.5 pts, let us know
what you think! (Be honest, no extra points for saying what you think we want to
hear ;)).

Write your comments into this file below this line, and include them in your submission.

Comments: Did it work?

-- Please fill in.


## Extra Point (1 pts)
If you got this far you likely won't need an extra point. But if you're curious,
you can implement a function that mines predicates from the program text and
uses them to eliminate predicate annotations. How many annotations can you
eliminate?

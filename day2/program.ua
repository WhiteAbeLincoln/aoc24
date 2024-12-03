# Experimental!

# read lines from stdin until closed, into a boxed array
Lines ← {◌ ⍢(&sc|type) &sc ◌}
# we want this to be a function, not a constant that runs immediately, so we discard the top of the stack
# &sc - do requires an initial value on the stack since it's a while loop, not a do while
# condition: type returns 0 for number, 1 for character array
# body: &sc - read line from stdin
# do - repeats a function while the condition holds, a while loop, not a do while
# pop - pops the final value from &sc, which will be 0

# split each line by spaces
Split ← ⊜□ ≠ @\s.
# first duplicate the input (top of stack) and execute (≠ @\s) on it.
# results in a mask array consisting of 1 & 0, with 1 where the character was equal to space
# this is the index array given to partition, which groups the second element in the stack (the input string)
# into a new array based on the indexes in index array. We pass the box function to partition since
# the split strings may have different lengths

# splits all strings and parses to numbers. boxes to handle rows with different lengths
ParseLines ← ⍚(⋕ Split)
# we use rows to run a function over every element of an array
# first unbox the element
# run Split on the result
# parse each element of the split string into numbers
# re-box to handle lines of different lengths

# for some reason we don't have a logical And or logical Or

# or can be defined using maximum. It is available using Experimental!,
# but not in the version I'm using
Or ← ↥

# define using not and or: A & B = ~(~A | ~B)
And ← ¬ Or ¬ : ¬

All ← ∧And : 1
Any ← ∧Or : 0

# now for our part 1 rules

# given an array of 2 numbers,
# takes the absolute value of the difference
# and checks that its >= 1 and <= 3
DiffInRange ← /(And ⊃(≥1) (≤3) ⌵ -)

# given a 'report', for each adjacent pair of levels
# checks if they are in range
LevelDiff ← All ≡DiffInRange ◫2

# checks if sorted decreasing by checking if input is equal to the reversed sorted input
SortedDecreasing ← ≍ ⇌ ⍆ .
# duplicate the input - [1 3 2] [1 3 2]
# sort the first on the stack - [1 2 3] [1 3 2]
# reverse the first on the stack - [3 2 1] [1 3 2]
# check if the arrays match - 0

# checks if sorted increasing by checking if input is equal to the sorted input
SortedIncreasing ← ≍ ⍆ .
# duplicate the input - [1 3 2] [1 3 2]
# sort the first on the stack - [1 2 3] [1 3 2]
# check if the arrays match - 0

# probably not the most efficient way
# we might be able to check the diff and the increasing decreasing
# in the same loop, but I don't know how to do that
# this way we have 3 seperate iterations over the row
SortedReport ← Or ⊃SortedDecreasing SortedIncreasing
SafeReport   ← And ⊃LevelDiff SortedReport

# Finally, now that we've defined our helper functions
# execute...

Input ← ParseLines Lines ""

# now count the number of safe reports from stdin
&pf "part 1: "
&p /+ ⍚SafeReport Input

# and now with the problem dampener, in the most inefficient way possible

# given a length N creates an NxN array consisting of 1s with a zero rotated through each position
# ? length
MissingMasks ← ≡↻ ⊙¤ ⟜(≠0) ⇡
# create array of length N - [0 1 2 3]
# create a mask with 0 at the front by calling (!= 0),
# but use on to keep the input on the stack - [0 1 2 3] [0 1 1 1]
# for each row of the ranged array, rotate the mask array

# creates an array of length N consisting of 1s
FilledMask ← ▽ : [1]
# combines MissingMasks and FilledMask using the given length
ArrayMask ← ⊂ ⊃MissingMasks FilledMask

# use keep with the array mask to remove one level from each report
WithMissing ← ≡(□ ▽) ⊃(ArrayMask ⧻) ¤

# and combine with Any
SafeReportDampener ← °□ Any ⍚(SafeReport) WithMissing

&pf "part 2: "
&p /+ ⍚SafeReportDampener Input

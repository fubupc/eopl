Exercise 1.11 [★] In the last line of subst-in-s-exp, the recursion is on sexp and not a smaller substructure. Why is the recursion guaranteed to halt?

**Answer:** Because it calls subst which recurs on smaller substructures.
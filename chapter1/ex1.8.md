Exercise 1.8 [★] In the deﬁnition of `remove-first`, if the last line were replaced by `(remove-first s (cdr los))`, what function would the resulting procedure compute? Give the contract, including the usage statement, for the revised procedure.

**Answer:**

**revised remove-first**: $Sym \times ListOf(Sym) \to ListOf(Sym)$
**usage**: `(remove-first s los)` remove all symbols from start to the first occurrence of symbol `s` in `los`.

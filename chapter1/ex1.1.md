Exercise 1.1 [★] Write inductive deﬁnitions of the following sets. Write each deﬁnition in all three styles (top-down, bottom-up, and rules of inference). Using your rules, show the derivation of some sample elements of each set.

1. $S=\{3n+2|n \in N\}$
	- top-down:
		1. $x=2$, or
		2. $x-3 \in S$.
	- bottom-up:
		1. $2 \in S$, and
		2. if $x \in S$, then $x+3 \in S$.
    - rules-of-inference:
        1. $2 \in S$
        2. $\displaystyle \frac{x \in S}{x+3 \in S}$
2. $S=\{2n+3m+1|n,m \in N\}$
    - top-down:
        1. $x=1$, or
        2. $x-2 \in S$, or
        3. $x-3 \in S$.
    - bottom-up:
        1. $1 \in S$, and
        2. if $x \in S$, then $x+2 \in S$,
        3. if $x \in S$, then $x+3 \in S$.
    - rules-of-inference:
        1. $1 \in S$
        2. $\displaystyle \frac{x \in S}{x+2 \in S}$
        3. $\displaystyle \frac{x \in S}{x+3 \in S}$
3. $S=\{(n,2n+1)|n \in N\}$
    - top-down:
        1. $(x, y)=(0,1)$, or
        2. $(x-1, y-2) \in S$.
    - bottom-up:
        1. $(0,1) \in S$, and
        2. if $(x, y) \in S$, then $(x+1, y+2) \in S$.
    - rules-of-inference:
        1. $(0,1) \in S$
        2. $\displaystyle \frac{(x,y) \in S}{(x+1,y+2) \in S}$
4. ${(n,n^2)|n \in N}$
    - top-down:
        1. $(x,y)=(0,0)$, or
        2. $(x-1,y-2x+1) \in S$.
    - bottom-up:
        1. $(0,0) \in S$, and
        2. if $(x,y) \in S$, then $(x+1,y+2x+1) \in S$.
    - rules-of-inference:
        1. $(0, 0) \in S$
        2. $\displaystyle \frac{(x,y) \in S}{(x+1,y+2x+1) \in S}$
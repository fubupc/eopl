Exercise 1.5 [`**`] Prove that if $e \in LcExp$, then there are the same number of left and right parentheses in $e$.

Deﬁnition 1.1.8 (lambda expression)
$$
\begin{aligned}
LcExp &::= Identiﬁer \\
      &::= (\text{lambda}\;(Identiﬁer)\;LcExp) \\
      &::= (LcExp\;LcExp)
\end{aligned}
$$
where an identiﬁer is any symbol other than `lambda`.

Proof:

Induction Hypothesis, $IH(k)$, is that any $e \in LcExp$ with $\le k$ left parentheses has the same number of right parentheses.

1. $IH(0)$: from definition, only production $LcExp ::= Identifier $ has no left parenthese. In this case, right parentheses is also 0, so $IH(0)$ holds.

2. Assume $IH(k)$ holds, that is, any $e \in LcExp$ with $\le k$ left parentheses has the same number of right parentheses. We need to prove $IH(k+1)$ holds. There are 3 cases according to the definition:

      1. $e ::= Idenfitier$: $e$ has 0 left and right parentheses.
      2. $e ::= (\text{lambda}\;(Identifier)\; e_1)$, where $e_1$ is a $LcExp$: Since $e$ has $n \le k+1$ left parentheses then $e_1$ must have $n-2 \le k-1$ left parentheses. By $IH(k)$, $e_1$ must have $n-2$ right parentheses. So $e$ has both $n$ left and right parenthese.
      3. $e ::= (e1\;e2)$. Since $e$ has $\le k+1$ left parentheses, $e1$ and $e2$ must have $n_1 \le k$ and $n_2 \le k$ left parentheses. By $IH(k)$, they must have $n_1$ and $n_2$ right parentheses. So $e$ has both $n_1+n_2+1$ left and right parentheses.
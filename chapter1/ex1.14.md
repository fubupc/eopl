Exercise 1.14 [★★] Given the assumption $0 ≤ n < length(v)$, prove that `partial-vector-sum` is correct.

**Proof:** `partial-vector-sum` is equivalent to the following funciton $f(n)$:

$$
f(n) = 
\begin{cases}
    v_0 & \quad n = 0\\
    v_n + f(n - 1) & \quad n > 0
\end{cases}
$$

The proof is by induction on $n$. The induction hypothesis, $IH(n)$, is $f(n) = \sum_{i=0}^{i=n} v_i$

1. $n = 0$: $f(0) = v_0$ so $IH(0)$ holds.
2. Let $0 <= n < length(v)-1$ such that $IH(n)$ holds, that is, $f(n) = \sum_{i=0}^{i=n}v_i$. Then for $n+1$, $f(n+1) = v_{n+1} + f((n+1) - 1) = v_{n+1} + f(n) = v_{n+1} + \sum_{i=0}^{i=n} v_i = \sum_{i=0}^{i=n+1} v_i$, so $IH(n+1)$ holds.
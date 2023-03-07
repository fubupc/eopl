Exercise 1.4 [*] Write a derivation from `List-of-Int` to `(-7 . (3 . (14 . ())))`.

$$
\begin{align*}
\gdef\ilist{\textit{List-of-Int}}
\gdef\cons{\enspace.\enspace}
            & \ilist \\
\implies & (Int\cons\ilist) \\
\implies & (-7\cons\ilist) \\
\implies & (-7\cons(Int\cons\ilist)) \\
\implies & (-7\cons(3\cons\ilist)) \\
\implies & (-7\cons(3\cons(Int\cons\ilist))) \\
\implies & (-7\cons(3\cons(14\cons\ilist))) \\
\implies & (-7\cons(3\cons(14\cons())))
\end{align*}
$$
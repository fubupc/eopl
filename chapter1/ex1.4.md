Exercise 1.4 [*] Write a derivation from `List-of-Int` to `(-7 . (3 . (14 . ())))`.

$$
\newcommand\ilist{\textit{List-of-Int}}
\newcommand\cons{\enspace.\enspace}
\begin{align*}
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

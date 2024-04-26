Exercise 2.4 [★★] Consider the data type of *stacks* of values, with an interface consisting of the procedures `empty-stack`, `push`, `pop`, `top`, and `empty-stack?`. Write a speciﬁcation for these operations in the style of the example above. Which operations are constructors and which are observers?

$$
\newcommand{\repr}[1]{\lceil #1 \rceil}
\begin{align*}
\mathtt{(empty\mbox{-}stack)} &= \repr{\emptyset} \\
\mathtt{(push}\; \repr{f}\; v \mathtt{)} &= \repr{g}, &&\text{ where } \mathtt{(top}\; \repr{g} \mathtt{)} = v \text{ and } \mathtt{(pop}\; \repr{g} \mathtt{)} = \repr{f} \\
\mathtt{(pop}\; \repr{f} \mathtt{)} &= \repr{g}, &&\text{ where } \mathtt{(push\; \repr{g}\; (top \repr{f}))} = \repr{f} \\
\mathtt{(top}\; \repr{f} \mathtt{)} &= v, &&\text{ where } \mathtt{(push\; (pop\; \repr{f})\; v)} = \repr{f} \\
\mathtt{(empty\mbox{-}stack?}\; \repr{f}\mathtt{)} &= 
    \begin{cases}
        \text{#t} &\text{if } f \text{ is } \emptyset \\
        \text{#f} &\text{otherwise}
    \end{cases}
\end{align*}
$$

Constructors: `empty-stack`, `push`.\
Observers: `empty-stack?`, `top` and `pop`. At first glance, `pop` should be a constructor instead of an observer. But if we view the stack as consisting of the top element and the remaining stack (another stack), then pop is actually about extracting the remaining stack rather than building a new stack.

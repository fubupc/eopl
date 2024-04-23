Exercise 2.4 [★★] Consider the data type of *stacks* of values, with an interface consisting of the procedures `empty-stack`, `push`, `pop`, `top`, and `empty-stack?`. Write a speciﬁcation for these operations in the style of the example above. Which operations are constructors and which are observers?

$$
\begin{align*}
(\text{empty-stack}) &= \lceil \emptyset \rceil \\
(\text{push}\; \lceil f \rceil\; v) &= \lceil g \rceil, \text{ where the top element of}\; g\; \text{is}\; v \\
(\text{pop}\; \lceil f \rceil) &= \lceil g \rceil, \text{ where } g \text{ is } f \text{ with the top element removed} \\
(\text{top}\; \lceil f \rceil) &= v, \text{ where } v \text{ is the top element of } f \\
(\text{empty-stack?}\; \lceil f \rceil) &= 
    \begin{cases}
        \text{\#t} & \text{if } f \text{ is } \emptyset \\
        \text{\#f} & \text{otherwise}
    \end{cases}
\end{align*}
$$

Constructors: `empty-stack`, `push`, `pop`.\
Observers: `empty-stack?`, `top`.
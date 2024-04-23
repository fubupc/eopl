Exercise 2.2 [★★] Analyze each of these proposed representations critically. To what extent do they succeed or fail in satisfying the speciﬁcation of the data type?

- Unary representation: Fully conforms. Allows arbitrarily large number (within limit of memory). Operation is slow, e.g. `plus` has complexity of `O(n)`.
- Scheme number representation: Not fully conforms. Nnly allows number within a limited range dependent on scheme number. Operation is fast.
- Bignum representation: Fully conforms. Allows arbitrarily large number. Operation is faster than unary representation but slower than scheme number representation.
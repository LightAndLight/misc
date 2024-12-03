# Polynomial Interpolation

*2024-11-28*

I stumbled across [Lagrange polynomials](https://en.wikipedia.org/wiki/Lagrange_polynomial) when learning about [Shamir's secret sharing](https://en.wikipedia.org/wiki/Shamir's_secret_sharing).
The [sops](https://github.com/getsops/sops) secret management tool uses Shamir's secret sharing to allow multiple different public keys (i.e. from different team members) to decrypt the same secret,
without encrypting the secret once per key.

A few things on the Wikipedia pages for Shamir's secret sharing and Lagrange polynomials caught my interest:

1. Lagrange interpolation is used in [Reed-Solomon error correction](https://en.wikipedia.org/wiki/Reed%E2%80%93Solomon_error_correction).

   These came up when I started learning about how QR codes work.

1. Shamir's secret sharing uses Lagrange interpolation over finite fields.

   Finite fields also came up when I was researching QR codes.

1. Lagrange interpolation has a linear algebra interpretation.

   I've slowly been working through Gilbert Strang's "Introduction to Linear Algebra",
   so I'm on the lookout for practical applications of linear algebra.

I feel like math is all too connected, and now I have to start learning it.
My favourite way of learning math is by solving computer science / computing problems,
and Shamir's secret sharing seems like a great jumping off point.

## References

* CS416: Introduction to Scientific Computing, University of Wisconsin-Madison

  Unit 3: Polynomial Interpolation, Feb 2007

  by Amos Ron, Yunpeng Li, Mark Cowlishaw, Steve Wright
  
  <https://pages.cs.wisc.edu/~swright/416/lectures/week03.pdf>

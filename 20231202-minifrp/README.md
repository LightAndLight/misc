# minifrp

*2023-12-02*

A small, simple (I hope) [push-pull FRP](http://conal.net/papers/push-pull-frp/) implementation.

In [`20231129-single-program-web-apps`](../20231129-single-program-web-apps) I compile FRP code to
JavaScript, but it had some mistakes that I didn't know how to fix.
I wrote `minifrp` as an attempt to understand how an FRP implementation should behave, in the hope
that I'll be able to improve the FRP compiler.
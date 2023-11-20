# Coeffects

*2023-11-19*

I've been aware of Tomas Petricek's work on coeffects[^1][^2] for a few years and have struggled to put it
into practise. I was reminded of its potential when reading [this post by Conal
Elliott](http://conal.net/blog/posts/notions-of-purity-in-haskell) about Haskell's
[`System.Info.os ::
String`](https://hackage.haskell.org/package/base-4.19.0.0/docs/System-Info.html#v:os).
`System.Info.os` doesn't really denote a string because if I evaluate it on my computer (I get `"linux"`) and
generally substitute `System.Info.os` for `"linux"` then I'll accidentally change the meaning of a
lot of programs. A simple example is the program
`(if System.Info.os == "linux" then 7 + 4 else 7 * 4) :: Integer`.
If I evaluate this on my NixOS installation, I get `11`, but on Windows I get `28`.
The meaning of this program depends on the system on which it's evaluated.
In contrast, `7 * 4 :: Integer` gives `28` no matter where I evaluate it.

I think context-dependency of values like `System.Info.os` should be described by coeffects.

* `Monad m => a -> m b` are `a -> b` with effect `m`
* `Comonad w => w a -> b` are `a -> b` with coeffect `w`
* `if System.Info.os == "linux" then 7 + 4 else 7 * 4` becomes
  `\(osInfo :: OsInfo a) -> if osName osInfo == "linux" then 7 + 4 else 7 * 4`
  * Perhaps even `if osName osInfo == "linux" then 7 + 4 else 7 * 4`, with `osInfo`a
    being global value. When it's a global value, it's like every definition has an extra `OsInfo
    ()` argument.
  * In distributed programs, `OsInfo a` can't be transferred between computers, but functions
    `OsInfo a -> b` can. A computer that recieves an `OsInfo a -> b` is forced to supply its native
    `OsInfo a` context.

[^1]: Petricek, T. (2017). Context-aware programming languages (No. UCAM-CL-TR-906). University of
Cambridge, Computer Laboratory.

[^2]: Petricek, T., Orchard, D., & Mycroft, A. (2014). Coeffects: a calculus of context-dependent computation. ACM SIGPLAN Notices, 49(9), 123-135.

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

I think the context-dependency of values like `System.Info.os` should be described by coeffects.
We use functions of type `Monad m => a -> m b` as `a -> b`s having effect `m`, and dually we can use
functions of type
`Comonad w => w a -> b` as `a -> b`s under coeffect `w`.
`if System.Info.os == "linux" then 7 + 4 else 7 * 4` becomes
`if osName osInfo == "linux" then 7 + 4 else 7 * 4` where `osInfo :: OsInfo a` and
`osName :: OsInfo a -> String`. `OsInfo` is a comonad, and an `OsInfo a` is an `a` that has been
under in the `OsInfo` context. While each platform provides a different `osInfo :: OsInfo ()`, the
meaning of `if osName osInfo == "linux" then 7 + 4 else 7 * 4`  stays the same.

## References

[^1]: Petricek, T. (2017). Context-aware programming languages (No. UCAM-CL-TR-906). University of
Cambridge, Computer Laboratory.

[^2]: Petricek, T., Orchard, D., & Mycroft, A. (2014). Coeffects: a calculus of context-dependent computation. ACM SIGPLAN Notices, 49(9), 123-135.

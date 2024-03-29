# `text-columns`

*2024-03-29*

[`util-linux`'s `column` command](https://man7.org/linux/man-pages/man1/column.1.html) seems to be
the de facto way of printing a list as columns. I tried to use it to "columnize" the output of
`ls --color=always --classify=always` and discovered that `column` doesn't account for
[ANSI color codes](https://gist.github.com/fnky/458719343aabd01cfb17a3a4f7296797#colors--graphics-mode)
([yet](https://unix.stackexchange.com/a/769660/50500)). I created this small project as a workaround,
because I realised that the column layout problem would be interesting.

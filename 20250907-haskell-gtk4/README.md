# Haskell GTK4

*2025-09-07*

A couple of programs from the [GTK4 tutorial](https://docs.gtk.org/gtk4/getting_started.html),
implemented using the [haskell-gi](https://github.com/haskell-gi/haskell-gi) ecosystem:

* [`haskell-gi-base`](https://hackage.haskell.org/package/haskell-gi-base)
* [`gi-gio`](https://hackage.haskell.org/package/gi-gio)
* [`gi-gobject`](https://hackage.haskell.org/package/gi-gobject)
* [`gi-gtk`](https://hackage.haskell.org/package/gi-gtk)

Among other things, I was curious about the final binary size. The simple examples weigh in
at ~90MB, compared to ~13MB for `main = putStrLn "Hello, world!"`.

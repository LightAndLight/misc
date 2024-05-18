val id = \x. x;

val false = \x, y. x;
val true = \x, y. y;
val ifte = \b, t, e. b(e, t);

val zero = \s, z. z;
val suc = \n. \s, z. s(n(s, z));

val three = 3;

(* This language only has "block" comments.

To mention the end-of-comment token symbol in a comment without ending the comment,
use a backslash followed by the end-of-comment token: \*).

To write a literal backslash, use two adjacent backslashes: \\.
*)

(* Bindings must be irreducible. The following are invalid:

```	
val threeChurch = suc(suc(suc(zero)));
val threePlus = 2 + 1;
val threeChurchPlus = three(\\n. n + 1, 0);
```
*)

(* Internally, comments are often attached to syntactic forms.

For example, a comment (such as this one) that ends on a line directly preceding
a definition is considered to annotate that definition.

This is intended to simplify documentation and literate programming tools.
*)
val const = \x, y. x;

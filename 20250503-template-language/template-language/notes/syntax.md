# Syntax

## Substitution

`{<expr>}`

Substitute an [expression](#expressions) into the template.

### `include`

`{% include <path> %}`

Substitute the template at `<path>` into the current template.

Any unsatisfied `require`ments (see [`require`](#require), [`define`](#define)) in the included template are inherited by the including template.

## Introducing and binding variables

### `require`

`{% require <name> [: <type>] %}`

The template requires an argument `<name>`.
`<name>` is exposed as a variable within the template.
If the `<type>` is omitted, then the variable has type `String`.

Unsatisfied `require`ments in a template become arguments when the template is run.

### `define`

`{% define <name> %}<fragment>{% end <name> %}`

The template defines a variable named `<name>` with value `<fragment>`.

If a template `require`s (inherited or not) a variable named `<name>`, then that requirement
is satisfied by `define <name>`.
A satisfied `require`ment is not visible outside the template in which it was satisfied.

If the `define`d variable is substituted (e.g. `{% define <name> %}<fragment>{% end <name> %}...{name}`)
then any unsatisfied `require`ments are inherited by its use-site.

#### Example (`define` satisfies `require`ment)

A single argument template:

```
$ cat > test.tpl.1 <<EOF
{% require name %}
Hello, {name}!
EOF

$ tpl reqs test.tpl.1
name

$ tpl run test.tpl.1
error: missing argument 'name'

$ tpl run test.tpl.1 --arg name "world"
Hello, world!
```

The same template, containing a `define` for the `require`d argument:

```
$ cat > test.tpl.2 <<EOF
{% require name %}
Hello {name}!
{% define name %}world{% end name}
EOF

$ tpl reqs test.tpl.2
(none)

$ tpl run test.tpl.2
Hello, world!

$ tpl run test.tpl.2 --arg name "world"
error: unexpected argument 'name'
```

#### Example (`require`ments in `define` are inherited by use-site)

```
$ cat > test.tpl <<EOF
{% define value %}{% require name %}{name}{% end value}
Hello, {value}!
EOF

$ tpl reqs test.tpl
name

$ tpl run test.tpl --arg name "world"
Hello, world!
```

```
$ cat > test.tpl <<EOF
{% require names : List %}
{% define greeting %}
{% require name %}
Hello, {name}!
{% end greeting %}
{for name in names yield {greeting}}
EOF

$ tpl reqs test.tpl
names

# tpl run test.tpl --arg-list names '["world1", "world2"]'
Hello, world1!
Hello, world2!
```

## Expressions

### Variables

`<name>`

Substitute the value of `<name>` into the template.

Variables are introduced by [`require`](#require) and [`define`](#define).

### List comprehensions

`for <name> in <expr> yield <fragment>`

Substitute many [template fragments](#template-fragments) into the template, one after the other.

For each item in `<expr>`, `<name>` is `define`d within the scope of `<fragment>` (see [Template fragments](#template-fragements)), with that item as its value.

### Template fragments

Some expression contexts (such as the `yield`ed value in a list comprehension) allow template fragments.
Anything that's permitted in a top-level template is also allowed in a template fragment.

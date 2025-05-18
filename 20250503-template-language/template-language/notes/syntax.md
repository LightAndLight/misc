# Syntax

* `{<expr>}`

  Substitute an expression into the template.

  ### Variables
  
  `{<name>}`

  Substitute the value of `<name>` into the template.

  ### List comprehensions

  `{for <name> in <expr> yield <fragment>}`

  Substitute many template fragments into the template, one after the other.

  For each item in `<expr>`, `<name>` is `define`d within the scope of `<fragment>`, with that item as its value.

  ### Template fragments

* `{% require <name> %}`

  The template requires an argument `<name>` of type `String`.

* `{% require <name> : <type> %}`

  The template requires an argument `<name>` of type `<type>`.

  Unsatisfied `require`ments in a template are inherited by parent templates (see `include`).
  `require`ments in a root template become arguments to the template.
  
  A `require`ment that is satisfied by a `define` is not visible outside the `define`ing template.

* `{% define <name> %}<fragment>{% end <name> %}`

  The template defines a variable named `<name>` with value `<fragment>`.

  If `<fragment>` contains `require`ments, then they are inherited wherever `<name>` is used.
  
  A `require`ment is in scope for `<name>`, then it is satified by the `define`.
  The `require`ment will no longer be visible outside the template that contains the `define`.

  ### Example

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

* `{% include <path> %}`

  Substitute an another template into the current template.

  Any unsatisfied `require`ments in the included template are inherited by the current template.

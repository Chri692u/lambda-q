# lambda-q
The abstract syntax of the language is the lambda calculus extended with notation for quoting expressions. Quoting is a mechanism for interpolating strings and combining expressions. The $\lambda_q$-calculus also introduces the set of core functions from the operating system along with a representation of the file system.

- `v` ∈ **Val**
- `c` ∈ **Con**
- `x` ∈ **Var**
- `e` ∈ **Exp**

Expressions `e` in the language can be defined as follows:
e ::= e1 e2 | λx.e | ⌊e⌋ | ⌈e⌉ | x | c | e1 ◦ e2 | e1 ; e2


Constants in the language can be described as **Con** = Σ* ∪ **OsCon** where **OsCon** is the set of builtin functions and constants from the underlying operating system. As an example, the set of builtins for Unix could be **OsCon** = { `mkdir`, `touch`, `cd`, `cwd`, `rm` }. The values `v` in the $\lambda_q$-calculus need to be defined:

- All constants are values, that is **Con** ⊆ **Val**
- All quoted expressions are values of the form ⌊e⌋
- All closures are values of the form ⟨x, e⟩

Now, the values of the language is defined as:
v ::= c | ⌊e⌋ | ⟨x, e⟩

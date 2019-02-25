# mind

for mind map

mind map is a tree. s expression is the best form to represent a tree.

# grammar

```
char := anything expect '\n' '(' ')'
term := <char> <term> | <char>
expression := <term> | '(' <term> '\n' <expression> ')'
```

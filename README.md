# mind

for mind map

mind map is a tree. s expression is the best form to represent a tree.

# grammar

```
normalChar := anything expect '\n' '(' ')'
term := <char> <term> | <char>
list :=  <expression> '\n' <list> | <expression>
expression := <term> | '(' <term> '\n' <list> ')'
```

# idea

s expression 作为思维导图的格式

parser.hs 将s expression 编译为 graphviz dot 使用dot输出图片

# editor

vim folder to fold mind map script

```
" fold for mind {{{
augroup filetype_mind
  autocmd!
  autocmd BufEnter,BufNew *.mind setlocal foldmethod=marker foldmarker=(,)
augroup END
" }}}
```

# commands

```bash
# from .mind to .dot
cat ./example/calculus.mind | ./parser > ./example/calculus.dot

# generate png
dot -Tpng ./example/calculus.dot -o calculus.png
```

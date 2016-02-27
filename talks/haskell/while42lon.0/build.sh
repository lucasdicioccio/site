
HsColour -lit -css > index.mkd < index.lhs
pandoc -sS --no-wrap --toc -c ./hsstyle.css > index.html < index.mkd

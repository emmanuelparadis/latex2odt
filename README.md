# latex2odt
This script takes a LaTeX+BiBTeX input and outputs an ODT file.

Requirements:
- (pdf)latex
- bibtex
- R
- pandoc

It works only if the .bbl file has the list of authors written as:

`Name1, X., Name2, Y., Name3, J. M., & Name4, Z. (2022).`

(i.e., it doesn't work with the Elseviers's .bst outputs).

If there are more than one authors with no given name (like in our paper Sudarto et al. 2010 in Mar. Biol.), this will be a problem.

Before running this script:
1. run (pdf)latex on $PREFIX
2. run bibtex on $PREFIX
3. run (pdf)latex again twice to solve the links/labels

Then run latex2odt.R which does the followings:
1. output azerty.tex and azerty.bbl
2. run pandoc on azerty.tex to produce azerty.odt
3. rename azerty.odt into $PREFIX.odt
4. delete azerty.tex and azerty.odt

The list of authors is shorten to `shortenAuthorsList` authors appended with `\emph{et al.}`.

`PREFIX` is the prefix of the file name (i.e., ms.tex by default).

*Note:* it seems that pandoc cannot handle colours specified in the .tex file (whatever the packages or commands used).

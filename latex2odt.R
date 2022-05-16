## This script takes a LaTeX+BiBTeX input and outputs an ODT file.

## Requirements: R, pdflatex, bibtex, pandoc

## It works only if the .bbl file is formatted with the list of authors as:

## Name1, X., Name2, Y., Name3, J. M., & Name4, Z. (2022).

## (i.e., it doesn't work with the Elseviers's .bst outputs).

## If there are more than one authors with no given name (like in our
## paper Sudarto et al. 2010 in Mar. Biol.), this will be a problem.

## The procedure is:
## 1. run pdflatex on $PREFIX
## 2. run bibtex on $PREFIX
## 3. run pdflatex again twice to solve the links/labels
## 4. run latex2odt.R to:
## 4.1. output 'azerty.tex' and azerty.bbl
## 4.2. run pandoc on azerty.tex to produce azerty.odt
## 4.3. rename azerty.odt into $PREFIX.odt
## 4.4. delete azerty.tex and azerty.odt

## The list of authors is shorten to 'shortenAuthorsList' authors
## appended with \emph{et al.}.

## PREFIX is the prefix of the file name (i.e., ms.tex by default).

shortenAuthorsList <- 6 # set to 0 for no shortening
PREFIX <- "ms"
authoryearseparator <-  " "
referenceseparator <-  ", "

myscan <- function(file, ...) {
    x <- scan(file, what = "", sep = "\n", quiet = TRUE, blank.lines.skip = FALSE, ...)
    paste(x, collapse = "\n")
}

mygrep <- function(pattern, x, fixed = TRUE)
    gregexpr(pattern, x, fixed = fixed)[[1]]

findnext <- function(what, after)
    what[sapply(after, function(x) which(what > x)[1])]

getsubstring <- function(x, from, to, strip = TRUE) {
    if (strip) {
        from <- from + 1L
        to <- to - 1L
    }
    mapply(substr, x = x, start = from, stop = to, USE.NAMES = FALSE)
}

TEX <- paste(PREFIX, "tex", sep = ".")
BBL <- paste(PREFIX, "bbl", sep = ".")

A <- myscan(TEX)
B <- myscan(BBL)

bibitem <- mygrep("\\bibitem", B)
leftbracket <- mygrep("[", B)
rightbracket <- mygrep("]", B)
leftbrace <- mygrep("{", B)
rightbrace <- mygrep("}", B)

LB <- findnext(leftbracket, bibitem)
RB <- findnext(rightbracket, LB)
LA <- findnext(leftbrace, RB)
RA <- findnext(rightbrace, LA)

KEY <- getsubstring(B, LA, RA)
tmp <- getsubstring(B, LB, RB)
YEAR <- gsub("^.*\\(|\\).*$", "", tmp)

if (any(grepl("natexlab", YEAR))) {
    YEAR <- gsub("{\\natexlab{", "", YEAR, fixed = TRUE)
    YEAR <- gsub("}}", "", YEAR, fixed = TRUE)
}
## maybe not needed

AUTHOR <- gsub("^\\{|\\(.*", "", tmp)

## transform .bbl file
for (i in seq_along(RA)) {
    n <- RA[i] - LB[i] + 1L
    z <- raw(n)
    z[] <- charToRaw("\r")
    substr(B, LB[i], RA[i]) <- rawToChar(z)
}
B <- gsub("\r", "", B, fixed = TRUE)
B <- gsub("\\bibitem", "\\item", B, fixed = TRUE)
## cat("\\section*{REFERENCES}\n", file = "azerty.bbl")
B <- gsub("\\\\begin\\{thebibliography\\}\\{[0-9]*\\}",
          "\\\\begin{enumerate*}\\\\setlength{\\\\itemsep}{0pt}", B)
B <- gsub("\\end{thebibliography}", "\\end{enumerate*}", B, fixed = TRUE)

## shorten list of authors... experimental!
if (shortenAuthorsList) {
    i <- nchar(B) # start from the end
    repeat {
        while (i > 6L && substr(B, i - 5L, i) != "\\item\n") i <- i - 1L
        ## find the next left parenthesis:
        k <- i + 1L
        while (substr(B, k, k) != "(") k <- k + 1L
        k <- k - 2L
        ## how many authors?
        tmp <- mygrep(",", B)
        commas <- tmp[tmp > i & tmp < k]
        Nauthors <- ceiling((2 + length(commas)) / 2)
        if (Nauthors > shortenAuthorsList) {
            j <- commas[2L]
            B <- paste0(substr(B, 1L, j - 1L),
                        "\\ \\emph{et al.}\\",
                        substr(B, k + 1L, nchar(B)))
        }
        if (i < 6L) break
        i <- i - 6L
    }
}

B <- gsub("WPP2019_Highlights", "WPP2019\\\\_Highlights", B)

## !!! pandoc ignores minipage !!!
#B <- gsub("\\\\begin\\{thebibliography\\}\\{[0-9]*\\}",
#          "\\\\begin{minipage}{.9\\\\linewidth}\n\\\\setlength{\\\\parindent}{-.1pt}", B)
#B <- gsub("\\end{thebibliography}", "\\end{minipage}", B, fixed = TRUE)
#B <- gsub("\\\\item", "", B)


CITE <- mygrep("\\\\cite[tp]", A, FALSE)
## overwrite the previous vectors:
leftbracket <- mygrep("[", A)
rightbracket <- mygrep("]", A)
leftbrace <- mygrep("{", A)
rightbrace <- mygrep("}", A)
LA <- findnext(leftbrace, CITE)
RA <- findnext(rightbrace, LA)

thekey <- getsubstring(A, LA, RA)
thekey <- gsub("\n", "", thekey, fixed = TRUE)
thecite <- getsubstring(A, CITE, LA, FALSE)

for (i in length(CITE):1) {
    tmp <- thekey[i]
    if (grepl(",", tmp)) tmp <- strsplit(tmp, ", *")[[1]]
    j <- match(tmp, KEY)
    if (grepl("citep", thecite[i])) {
        x <- paste(AUTHOR[j], YEAR[j], sep = authoryearseparator)
        if (length(x) > 1) x <- paste(x, collapse = referenceseparator)
        x <- paste0("(", x, ")")
    }
    if (grepl("citet", thecite[i])) {
        yrs <- YEAR[j]
        author <- AUTHOR[j]
        if (length(yrs) > 1) {
            yrs <- paste(yrs, collapse = ", ")
            if (length(unique(author)) > 1)
                warning("different authors in \\citet{... Taking the first one(s).")
            author <- author[1]
        }
        x <- paste0(author, " (", yrs, ")")
    }
    if (length(x) > 1) browser()
    A <- paste0(substr(A, 1L, CITE[i] - 1L), x, substr(A, RA[i] + 1L, nchar(A)))
}

### !!! the two lines below need to be made more general !!! ###

## a1 <- mygrep("\\setlength{\\bibsep}{0pt}", A)
a1 <- mygrep("\\bibliographystyle", A)
## a2 <- mygrep("outfile,outfile}", A)
## a2 <- a2 + attr(a2, "match.length")
a2 <- findnext(mygrep("}", A), mygrep("\\bibliography{", A))

A <- paste0(substr(A, 1, a1 - 1L),
            "\\section{References}\n\\include{azerty.bbl}\n\n",
            substr(A, a2 + 1L, nchar(A)))

cat(A, file = "azerty.tex")
cat(B, file = "azerty.bbl")
system("pandoc --from=latex --to=odt -i azerty.tex -o azerty.odt -s")
file.rename("azerty.odt", paste(PREFIX, "odt", sep = "."))

unlink(c("azerty.tex", "azerty.bbl"))

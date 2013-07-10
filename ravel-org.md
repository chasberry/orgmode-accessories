
# Background



## browsing this file

`ox-ravel.org` is an orgmode file. 

(`ravel.org` is the **old** version that supports the development setup before
the February 2013 merge of the new exporter into the master. Refer to
it if you still use that version of `orgmode`.)

If you are browsing `github`, you
should probably look at the file `ravel-org.md` which is formatted for viewing
(and omits lots of details that are usually not of interest). 

If you have downloaded a copy of `ox-ravel.org`, it is best to view it using orgmode

## quickstart guide

If you already know `orgmode` and either `knitr`, `Sweave`, or
`brew`, you can just

-   skip to "lisp setup"

-   browse one or more examples

-   review the section "what ravel does"

## orgmode

[Orgmode](http://orgmode.org/index.html) is *for keeping notes, maintaining TODO lists, doing project planning, and authoring with a fast and effective plain-text system.* The same org document can be used to create LaTeX, HTML, plain ASCII, and various other formats upon export.

[Babel](http://orgmode.org/worg/org-contrib/babel/index.html) is *Org-mode's ability to* *execute source code* *within Org-mode documents*. It is powerful and flexible and a useful tool for generating reproducible research reports as outlined in [the report :](http://www.jstatsoft.org/v46/i03) Eric Schulte, Dan Davison, Thomas Dye, and Carsten Dominik. A Multi-Language Computing Environment for Literate Programming and Reproducible Research. Journal of Statistical Software. 46(3) Jan 2012. Source code blocks can be editted in place or in a language major-mode edit buffer containing the body of the code block. And for R blocks, `ESS` is fully functional.

## R document/report Generators

There are many markup and report and document generating packages and
functions for R. A useful overview of them is at [CRAN Task View for
Reproducible Research](http://cran.r-project.org/web/views/ReproducibleResearch.html)

Several to which this software is aimed are 

-   **Sweave:** This R function is the basic workhorse for many report
    and document generating functions. It uses (by default)
    a noweb type syntax to separate code chunks from LaTeX
    markup and can be run to *weave* reports or *tangle*
    executable code.. It described in
    detail at
    
    -   [the Sweave homepage](http://www.statistik.uni-muenchen.de/~leisch/Sweave/) and in
    
    -   Friedrich Leisch. Sweave: Dynamic generation of statistical
        reports using literate data analysis. In Wolfgang Härdle and
        Bernd Rönz, editors, Compstat 2002 - Proceedings in
        Computational Statistics, pages 575-580. Physica Verlag,
        Heidelberg, 2002. ISBN 3-7908-1517-9.


-   **brew:** This R package uses a simple PHP-like syntax to markup
    documents mixing text and code which are then through
    the `brew()` function. It is described in the help documents for the package: 
    
    -   Jeffrey Horner (2011). brew: Templating Framework for Report
        Generation. R package version 1.0-6.
        <http://CRAN.R-project.org/package=brew>

-   **knitr:** This R package is an attempt to combine and unify the
    best features of many report generating/markup
    package and functions in R. It is described in
    
    -   Yihui Xie (2012). knitr: A general-purpose package for
        dynamic report generation in R. R package version 0.6.3.
        <http://CRAN.R-project.org/package=knitr> and
    
    -   [The knitr home page](http://yihui.name/knitr/)

## Putting orgmode and Sweave together

Each approach (orgmode as report generator vs Sweave-like report
generators) has its own strengths and weaknesses.

After years of using Sweave for report creation, I tried to use
orgmode to construct a research report from a moderate sized database
using some simulations and lengthy statistical resampling routines. I
got through the first draft of the report and a 25-page supplement
using orgmode/Babel.

I really liked the ease with which I could organize some things in the
process - like notes, emails from collaborators, TODO lists, and code
snippets and intermediate results that might not show up in the final
report or supplement or whose location was uncertain. Being able to
run code snippets and have the results saved in the master document
and be able to toggle to view LaTeX snippets and graphics rendered in
place was very slick.

However, there were some things I really missed that were helpful in
the Sweave style of work. There are *dependency aware* caching systems
in R that are simple and powerful; you can change a line of code and
rerun the document and all the pieces that need to be recomputed are
rebuilt and recached. And it all happens in a flash if all you needed
to do was change a format in table or anything that doesn't require a
large object to be rebuilt. And when a large object is rebuilt, all
its dependencies get updated, too. I ended up building a collection of
intermediate R objects to make the build/revision process execute in
finite time, but maintaining them and rebuilding them as needed was a
nuisance. Some componenents of the project required only a subset of
objects and the R packages that access them and I ended up having to
sprinkle links to the code to `load` or `require` throughout the
orgmode file. In the end, the orgmode files that served as the master
and the supplement were more than 7500 lines long and had more than
130 R source code blocks. And to be honest, there were some
stand-alone R scripts and a separate org file that had over 100 source
code blocks in to to manange the CPU intensive computations. It ended
up being pretty ungainly.

Also, there are some nice formatting tools available and more coming
into use. And many of them are hard (or maybe impossible) to use when
Babel is doing the final report generation. I did use `brew` under
Babel, but it was truly an awkward process.

I see terrific possibilities presented by new (as of July 2012) R
packages in development like `knitr` and [`pander`](https://github.com/daroczig/pander). It is tempting to
switch to those tools for my next project, but I want to retain the
features of orgmode+Babel for my workflow. The new export engine (see
[Org Export Reference Documentation](http://orgmode.org/worg/dev/org-export-reference.html)) offers the possibility to have a
workstyle in which an orgmode master document contains a *subtree*
with text and code blocks that when exported becomes an Sweave or brew
style document that when run produces LaTeX, HTML, and/or some other
markup language. By including caching options in those documents the
development process becomes easy to organize, restarting work after a
break is just a matter of rerunning the document to load the cached
objects and libraries, then switching to the orgmode master to try out
some new code, reorganize the presentation, and so on.

# lisp setup


**Warning:** As of <span class="timestamp-wrapper"><span class="timestamp">[2013-02-06 Wed]</span></span> the new exporter replaced the old
exporter in the git master. `ox-ravel.org` works with that
version. `ravel.org` is deprecated and will fail to work with the
new git or the next formal release of `orgmode`.


## Load the new exporter files

`ox-ravel.el` will take care of this step, so if you go to the next
step - **extract ox-ravel.el and load it** , it should *just work*.

`ox` is required and at least one of the
defined backends. Here I use `latex`, `html`, and `md`:

    (require 'ox-latex)
    (require 'ox-html)
    (require 'ox-md)

## extract ox-ravel.el and load it

These two lines should do the trick:

    (org-babel-tangle)
    (load-file "ox-ravel.el")

Eventually, move `ox-ravel.el` into your load path, e.g. `~/.emacs.d/` or `~/elisp/`
and add `(require 'ox-ravel)` to your startup.

# examples

Some `*.org` files in this directory show how the available backends
can be used. Check out

-   `test-cases.org`

-   `example-1-Rnw.org`

-   `knitr-minimal-rhtml.org`

# ravel




## using and extending ravel

### what ravel does

`ravel` allows exporting `*.org` files or subtrees in them to several
reproducible research formats: `brew`, `Sweave`, and several `knitr`
formats (brew, Rnw, Rmd, and Rhtml).

With `ox-ravel-el` loaded and the point in a `*.org` buffer, 

    M-x org-export-dispatch RET

will pop up a menu of choices. Among these are keys to allow export to
one of the formats supported by `ravel`. Alternatively, the user can
execute elisp code using the `org-export-to-file` and
`org-export-to-buffer` functions.

`ravel` exports the file or subtree in a suitable format
(currently LaTeX, HTML, or Markdown), but with differences from the
usual export mechanism in which the source code (aka src blocks) are
evaluated by orgmode's Babel engine and (optionally) code and/or
results are passed to the exporter.

Before the document is parsed, Babel is run. However, R src blocks are
not evaluated. Instead they are passed on to the exporter as is except
that when the `:noweb yes` header argument is given, the expanded src
block is created (by inserting the code from the blocks in the noweb
references) and passed to the exporter. R src blocks that have the
`:exports none` header argument are not exported. Src blocks in other
languages are evaluated and exported as usual.

Thus, a document can define many R src blocks and select a few to
export by constructing a subtree with src blocks that include noweb
references in them and for which `:noweb yes` is specified. Then, just
that subtree can be exported. 

Once Babel is finished, the exporter takes over. Any R header
arguments named `ravel` are passed to the exporter for use as
options in the ultimate code chunks. So, `knitr` chunk options such
as 'results="as.is"' would be given as `ravel` arguments. The way
these are handled depends on the backend; for `knitr` they are placed
as chunk options and for `brew` they are used to construct variants
of the '<% &#x2026; %>' code delimiters.

### existing backends

Currently, backends are avaiable for 

-   **`latex-noweb`:** LaTeX Sweave or knitr documents

-   **`latex-brew`:** LaTeX brew templates

-   **`beamer-noweb`:** beamer Sweave or knitr slides

-   **`html-knitr`:** HTML knitr documents

-   **`md-knitr`:** Markdown knitr documents

-   **`md-brew`:** Markdown brew templates

A look at the `*.org` files in the examples section should provide a
quickstart.  A look (below) at the definitions of the style functions
for these backends should guide further devlopment.

### explicit specification of arguments in exported chunks

Arguments that need to be passed to exported code chunks can be placed
after a `:ravel` key in a `#+begin_src R` line. Or they can be given
in `#+ATTR_RAVEL:` lines immediately before the src block. 

Some care is needed. Arguments for some backends may conflict with
other backends. In future development, it might help to prefix
arguments with the name of their backend.

### using Babel header arguments in exported code chunks

Babel header arguments are parsed and (the alist of such arguments is)
made available to the `org-ravel-chunk-style-BACKEND` function as
`r-headers-attr`. This will eventually allow translation of some
native org-babel headers to exported chunk headers. For this to
happen, the chunk style of a backend (say `latex-newstyle`) such as this

    (defun org-ravel-chunk-style-latex-newstyle 
      (label ravel r-headers-attr src-code)
      ( ... ))

needs to inspect the alist of `r-headers-attr` and find those that can
be (re-)rendered and add the necessary arguments to the output string
in the header position along with the arguments provided by the
`ravel` argument.

### new backends

It is fairly easy to add more backends. There are these
ingredients needed:

1.  chunk style function

2.  inline style function

3.  a call to setup up the derived backend

4.  (optionally) a function to work with the `org-export-dispatch` menu

The examples below should serve to show what is needed to create
different chunk and inline styles.

1.  defining a derived backend

    This template will probably be the same for all output styles - Sweave,
    brew, knitr variants, etc. The actual definition of the styles
    requires two style specific functions be created:
    
    -   **org-ravel-chunk-style-BACKEND:** Is the src block chunk style to be used
        for BACKEND, where 'BACKEND' is the name of the backend. The
        arguments of this function are described in more detail below.
    
    -   **org-ravel-inline-style-BACKEND:** Is the inline src block style to be used
        for BACKEND, where 'BACKEND' is the name of the backend.
    
    The template-alist must be specified as:
    
        #+NAME: defineDerived
        #+begin_src emacs-lisp :tangle ox-ravel.el
            
          (org-export-define-derived-backend BACKEND PARENT
            :translate-alist ((src-block . org-ravel-src-block)
                              (inline-src-block . org-ravel-inline-src-block)
                              ))
          
          
        #+end_src
    
    where 'BACKEND' is the name of the new backend and 'PARENT' is the
    name of the original backend, e.g. "latex".
    
    The derived backends can inherit their *family name* and append the
    chunk style to it, e.g. `latex-noweb` for latex files using the
    traditional noweb style chunks.
    
    Optionally, a `:menu-entry` list can be added to make use of the menu
    based `org-export-dispatch` function. See the examples and the
    docstring for `org-export-define-backend` for more information.
    
    The parent of the derived backend must be loaded to enable access to
    its functions when the derived backend is executed. The `require`
    statements in the lisp setup (See section 2)

## latex-noweb backend

This backend produces an Sweave noweb style document.

### template-alist

    (org-export-define-derived-backend 'latex-noweb 'latex
      :translate-alist '((src-block . org-ravel-src-block)
                        (inline-src-block . org-ravel-inline-src-block))
      :menu-entry
      '(?l 1
          ((?r "As Rnw File" org-ravel-latex-noweb-dispatch))))

### chunk style

    (defun org-ravel-chunk-style-latex-noweb 
      (label ravel r-headers-attr src-code)
      "Chunk style for noweb style.
    LABEL is the chunk name, RAVEL is the collection of ravel args as
    a string, R-HEADERS-ATTR is the collection of headers from Babel
    as a string parseable by `org-babel-parse-header-arguments',
    SRC-CODE is the code from the block."
      (concat
       "<<" label
       (if (and ravel label) ",") ravel ">>=\n"
       src-code
       "@ %def\n"))

### inline src style

    (defun org-ravel-inline-style-latex-noweb 
      (inline-src-block contents info)
      "Traditional Sweave style Sexpr using the INLINE-SRC-BLOCK element.
    CONTENTS holds the contents of the item.  INFO is a
    plist holding contextual information."
      (format "\\Sexpr{ %s }" (org-element-property :value inline-src-block)))

### export dispatcher

See the help string for `org-export-dispatch`, which invokes this
function. The `l r` menu options invoke ravel dispatch to produce a
Rnw file. The file name is the basename of the buffer file or for
subtree export the `EXPORT_FILE_NAME` property is used, if it is
set. In short:

-   To turn a buffer into knitr Rnw file, type
    
        M-x org-export-dispatch RET l r

-   For a subtree, place point under its headline, type
    
        M-x org-export-dispatch RET C-s l r


    (defun org-ravel-latex-noweb-dispatch 
      (&optional async subtreep visible-only body-only ext-plist)
    "Execute menu selection. See org-export.el for meaning of ASYNC,
          SUBTREEP, VISIBLE-ONLY and BODY-ONLY."
    (interactive)
    (if async
        (message "No async allowed.")
      (let
          ((outfile  (org-export-output-file-name ".Rnw" subtreep)))
           (org-export-to-file 'latex-noweb 
                               outfile subtreep visible-only 
                               body-only ext-plist))))


The `minimalRnw` class provides a simple header for the `article`
documentclass. It is suitable for `knitr` runs.

To use this header, put

    #+LaTeX_CLASS: minimalRnw

near  the top of the file or as an `EXPORT_LATEX_CLASS` property in a
subtree.

    (unless 
        (assoc "minimalRnw" org-latex-classes)
      (let    
          ((art-class (assoc "article" org-latex-classes))
           (headstring "\\documentclass{article}\n[NO-DEFAULT-PACKAGES]\n\\usepackage{hyperref}"))
        (add-to-list 'org-latex-classes
                     (append 
                      (list "minimalRnw"
                            headstring)
                      (cddr 
                       (assoc "article" 
                              org-latex-classes))))))

## beamer-noweb backend

### template alist

Note here the :latex-class is forced to "beamer" by default - I hate
using LATEX<sub>CLASS</sub> in the file as it bleeds into subtrees that are not
being exported.

    (org-export-define-derived-backend 'beamer-noweb 'beamer
      :translate-alist '((src-block . org-ravel-src-block)
                        (inline-src-block . org-ravel-inline-src-block))
      :menu-entry
      '(?l 1
          ((?s "As Rnw Beamer" org-ravel-beamer-noweb-dispatch))))

### chunk style

This adds "%" before the code chunk to defeat beamer's tinkering
with contents which would add a space before a chunk (and thereby
disarm Sweave/knitr's processing of it)

    (defun org-ravel-chunk-style-beamer-noweb
       (label ravel r-headers-attr src-code)
       "Chunk style for noweb style.
     LABEL is the chunk name, RAVEL is the collection of ravel args as
     a string, R-HEADERS-ATTR is the collection of headers from Babel
     as a string parseable by `org-babel-parse-header-arguments',
     SRC-CODE is the code from the block."
       (concat
        "%\n<<" label
        (if (and ravel label) ",") ravel ">>=\n"
        src-code
        "@ %def\n"))

### inline src style

    (defalias 'org-ravel-inline-style-beamer-noweb 'org-ravel-inline-style-latex-noweb)

### export dispatcher

See the comments for `latex-noweb`.

-   To turn a buffer into knitr beamer file, type
    
        M-x org-export-dispatch RET l s

-   For a subtree, place point under its headline, type
    
        M-x org-export-dispatch RET C-s l s


    (defun org-ravel-beamer-noweb-dispatch 
      (&optional async subtreep visible-only body-only ext-plist)
    "Execute menu selection. See org-export.el for meaning of ASYNC,
          SUBTREEP, VISIBLE-ONLY and BODY-ONLY."
    (interactive)
    (if async
        (message "No async allowed.")
      (let
          ((outfile  (org-export-output-file-name ".Rnw" subtreep)))
           (org-export-to-file 'beamer-noweb 
                               outfile subtreep visible-only 
                               body-only ext-plist))))


The `minimalRnw` class provides a simple header for the `article`
documentclass. It is suitable for `knitr` runs.

To use this header, put

    #+LaTeX_CLASS: beamer

near  the top of the file or as an `EXPORT_LATEX_CLASS` property in a
subtree.

    (unless
        (assoc "beamer" org-latex-classes)
      (add-to-list 
       'org-latex-classes  
       (append (list
                "beamer"
                "\\documentclass[11pt]{beamer}")
               (cddr (assoc "article" org-latex-classes)))))

## latex-brew backend

### brew document format


The [brew](http://cran.r-project.org/web/packages/brew/index.html) R package use "<%" and "%>" to delimit code
chunks. Placing an equals sign (\`=') after the first delimiter
will result in the value of the expression being printed in place
of the code chunk. Placing a minus sign (\`-') before the last
delimiter will suppress the line break after the code chunk.

It is handy to be able to evaluate the code inside the delimiters
during development and debugging using the `C-c C-c` key, but this
can only be done on complete, syntactically correct R
expressions. In orgmode, this can be achieved by letting the
delimiters live outside of the code chunk as an extra
argument. Here is an example


    #+begin_src R :ravel <% code %> 
      load("my-data.RData")
    #+end_src
    
    #+RESULTS:
    
    
    #+begin_src R :ravel <%= code %>
      cat( print( ls() ), sep="\n")
    #+end_src

The code in each chunk can be executed via `C-c C-c`. On export,
it is wrapped in the delimiters. The text `code` is deleted. (In
fact, it need not be there at all.)

One of the nifty features of `brew` is that the code chunks do not
need to be complete expressions. Thus, one can use

    The alphabet:
    <% for (i in letters) { %>
    <%= c( i, toupper(i) ) %>
    <% } %> 

to print the letters of the alphabet. In orgmode, the exporter
becomes confused by code chunks like `for (i in letters)
    {`. Allowance for this idiom is made by placing the opening or
closing curly brace just before the last delimiter (\`[-]%>') like
this `<% } -%>`. The curly brace will appear after the code (if
any) in the chunk after export.

### template-alist

    (org-export-define-derived-backend 'latex-brew 'latex
       :translate-alist '((src-block . org-ravel-src-block)
                         (inline-src-block . org-ravel-inline-src-block))
       :menu-entry
       '(?l 2
          ((?w "As Brew File" org-ravel-latex-brew-dispatch))))

### chunk style

    (defun org-ravel-chunk-style-latex-brew 
      (label ravel r-headers-attr src-code)
      "Default chunk style for brew style.
    LABEL is the chunk name,RAVEL is the collection of ravel args as a
    string,R-HEADERS-ATTR is the collection of headers from Babel as
    a string parseable by `org-babel-parse-header-arguments',SRC-CODE
    is the code from the block."
        (format (org-ravel-format-brew-spec ravel) src-code))

### inline style

    (defun org-ravel-inline-style-latex-brew 
      (inline-src-block contents info)
      "Traditional brew style using the INLINE-SRC-BLOCK element.
    CONTENTS holds the contents of the item.  INFO is a plist holding
    contextual information."
      (format (org-ravel-format-brew-spec
               (or
                (org-element-property :parameters inline-src-block)
                "<%= code -%>"))
              (org-element-property :value inline-src-block)))

### brew formatting :ravel arguments

A function is needed to check the spec, escape percent signs, and
return a format STRING that is suitable for brew.

    (defun org-ravel-format-brew-spec (&optional spec)
      "Check a brew SPEC, escape % signs, and add a %s spec."
      (let
          ((spec (or spec "<% %>")))
        (if (string-match 
             "<\\(%+\\)\\([=]?\\)\\(.+?\\)\\([{}]?[ ]*-?\\)\\(%+\\)>" 
             spec)
            (let (
                  (opct (match-string 1 spec))
                  (eqsign (match-string 2 spec))
                  (filler (match-string 3 spec))
                  (enddash (match-string 4 spec))
                  (clpct (match-string 5 spec)))
              (if (string= opct clpct)
                  (concat "<" opct opct eqsign " %s " enddash clpct clpct ">")
                (error "Percent signs do not balance:%s" spec)))
          (error "Invalid spec:%s" spec))))

### export dispatcher

See the comments for `latex-noweb`. The `minimalRnw` class can be
used here, too.

-   To turn a buffer into latex brew file, type
    
        M-x org-export-dispatch RET l w

-   For a subtree, place point under its headline, type
    
        M-x org-export-dispatch RET C-s l w

    (defun org-ravel-latex-brew-dispatch 
      (&optional async subtreep visible-only body-only ext-plist)
    "Execute menu selection. See org-export.el for meaning of ASYNC,
          SUBTREEP, VISIBLE-ONLY and BODY-ONLY."
    (interactive)
    (if async
        (message "No async allowed.")
      (let
          ((outfile  (org-export-output-file-name ".brew" subtreep)))
           (org-export-to-file 'latex-brew 
                               outfile subtreep visible-only 
                               body-only ext-plist))))

## html-knitr backend

This produces an html style document as supported by [knitr.](http://yihui.name/knitr/) 

### template-alist

    (org-export-define-derived-backend 'html-knitr 'html
      :translate-alist '((src-block . org-ravel-src-block)
                        (inline-src-block . org-ravel-inline-src-block))
       :menu-entry
       '(?h 3
          ((?r "As Rhtml File" org-ravel-html-knitr-dispatch))))

### chunk style

    (defun org-ravel-chunk-style-html-knitr 
      (label ravel r-headers-attr src-code)
      "Chunk style for noweb style.
    LABEL is the chunk name, RAVEL is the collection of ravel args as
    a string, R-HEADERS-ATTR is the collection of headers from Babel
    as a string parseable by `org-babel-parse-header-arguments',
    SRC-CODE is the code from the block."
      (concat
       "<!--begin.rcode "
       label
       (if (and ravel label) ",") ravel "\n"
       src-code
       "end.rcode-->\n"))

### inline src style

    (defun org-ravel-inline-style-html-knitr 
      (inline-src-block contents info)
      "Traditional Sweave style Sexpr using the INLINE-SRC-BLOCK element.
    CONTENTS holds the contents of the item.  INFO is a
    plist holding contextual information."
      (format "<!--rinline %s -->" (org-element-property :value inline-src-block)))

### export dispatcher

See the comments for `latex-noweb`.

-   To turn a buffer into Rhtml file, type
    
        M-x org-export-dispatch RET h r

-   For a subtree, place point under its headline, type
    
        M-x org-export-dispatch RET C-s h r

    (defun org-ravel-html-knitr-dispatch 
      (&optional async subtreep visible-only body-only ext-plist)
    "Execute menu selection. See org-export.el for meaning of ASYNC,
          SUBTREEP, VISIBLE-ONLY and BODY-ONLY."
    (interactive)
    (if async
        (message "No async allowed.")
      (let
          ((outfile  (org-export-output-file-name ".Rhtml" subtreep)))
           (org-export-to-file 'html-knitr 
                               outfile subtreep visible-only 
                               body-only ext-plist))))

## md-knitr backend

This backend produces a Markdown style document as supported by [knitr.](http://yihui.name/knitr/) 

### template-alist

    (org-export-define-derived-backend 'md-knitr 'md
      :translate-alist '((src-block . org-ravel-src-block)
                        (inline-src-block . org-ravel-inline-src-block))
      :menu-entry
      '(?m 4
          ((?r "As Rmd (Markdown) File" org-ravel-md-knitr-dispatch))))

### chunk style

    (defun org-ravel-chunk-style-md-knitr 
      (label ravel r-headers-attr src-code)
      "Chunk style for markdown.
    LABEL is the chunk name, RAVEL is the collection of ravel args as
    a string, R-HEADERS-ATTR is the collection of headers from Babel
    as a string parseable by `org-babel-parse-header-arguments',
    SRC-CODE is the code from the block."
      (concat
       "```{r "
       label
       (if (and ravel label) ",") ravel "}\n"
       src-code
       "```\n"))

### inline src style

    (defun org-ravel-inline-style-md-knitr 
      (inline-src-block contents info)
      "Markdown style Sexpr using the INLINE-SRC-BLOCK element.
    CONTENTS holds the contents of the item.  INFO is a
    plist holding contextual information."
      (format "`r %s`" (org-element-property :value inline-src-block)))

### export dispatcher

See the comments for `latex-noweb`.

-   To turn a buffer into Markdown `rmd` file, type
    
        M-x org-export-dispatch RET m r

-   For a subtree, place point under its headline, type
    
        M-x org-export-dispatch RET C-s m r

    (defun org-ravel-md-knitr-dispatch 
      (&optional async subtreep visible-only body-only ext-plist)
    "Execute menu selection. See org-export.el for meaning of ASYNC,
          SUBTREEP, VISIBLE-ONLY and BODY-ONLY."
    (interactive)
    (if async
        (message "No async allowed.")
      (let
          ((outfile  (org-export-output-file-name ".Rmd" subtreep)))
           (org-export-to-file 'md-knitr 
                               outfile subtreep visible-only 
                               body-only ext-plist))))


## md-brew backend

This backend produces a Markdown style [brew](http://cran.r-project.org/web/packages/brew/index.html) template. There are
notes about the use of the template above in the
brew document format (See section 4.4.1) section.

### template-alist

    (org-export-define-derived-backend 'md-brew 'md
      :translate-alist '((src-block . org-ravel-src-block)
                        (inline-src-block . org-ravel-inline-src-block))
      :menu-entry
      '(?m 4
          ((?w "As brew (Markdown) File" org-ravel-md-brew-dispatch))))

### chunk style

The function `org-ravel-chunk-style-latex-brew` (defined above)
works.

    (defalias 'org-ravel-chunk-style-md-brew  
      'org-ravel-chunk-style-latex-brew)

### inline src style

The function `org-ravel-inline-style-latex-brew` (defined above)
works.

    (defalias 'org-ravel-inline-style-md-brew  
      'org-ravel-inline-style-latex-brew)

### export dispatcher

See the comments for `latex-brew`.

-   To turn a buffer into a Markdown `brew` template file, type
    
        M-x org-export-dispatch RET m w

-   For a subtree, place point under its headline, type
    
        M-x org-export-dispatch RET C-s m w

    (defun org-ravel-md-brew-dispatch 
      (&optional async subtreep visible-only body-only ext-plist)
    "Execute menu selection. See org-export.el for meaning of ASYNC,
          SUBTREEP, VISIBLE-ONLY and BODY-ONLY."
    (interactive)
    (if async
        (message "No async allowed.")
      (let
          ((outfile  (org-export-output-file-name ".brew" subtreep)))
           (org-export-to-file 'md-brew 
                               outfile subtreep visible-only 
                               body-only ext-plist))))


## support functions

These functions should not be changed for individual backends.

### src block ancestors

Non-R src blocks should use the src-block and inline-src-block functions of
the parent (e.g. latex). This function helps to find them.


      (defun org-ravel-get-ancestor-fun (funkey &optional info)
    "Ancestral definition of function.
    Find  second or sole FUNKEY function in the `:translate-alist' property of INFO."
    (let* ((tr-list (plist-get  info :translate-alist))
           (newfun-pair (assoc funkey tr-list))
           (new-and-rest (memq newfun-pair tr-list)))
      (or
       (cdr (assoc funkey (cdr new-and-rest)))
       (cdr newfun-pair))))

### R src block transcoder

This function looks for a function name org-ravel-chunk-style-BACKEND, where
BACKEND is the name of the backend. If it funds one and if an R src
block is being processed, then it calls that function with args (label
ravel code). This function should not be modified by users.

        (defun org-ravel-src-block (src-block contents info)
          "Transcode a SRC-BLOCK element.
    CONTENTS holds the contents of the item.  INFO is a plist
    holding contextual information.  If org-ravel-chunk-style-BACKEND
    is defined, that will be called for R src blocks."
          (let* ((lang (org-element-property :language src-block))
                 (label (org-element-property :name src-block))
                 (ravel-attr (org-element-property :attr_ravel src-block))
                 (r-headers-attr (org-element-property :attr_r-headers src-block))
                 (ravel (if ravel-attr
                            (mapconcat #'identity ravel-attr ", ")))
                 (bkend (org-export-backend-name (plist-get info :back-end)))
                 (chunk-style-fun (intern (concat "org-ravel-chunk-style-" 
                                                  (symbol-name bkend)))))
            (if (and (string= lang "R") (fboundp chunk-style-fun))
                (funcall chunk-style-fun label ravel r-headers-attr
                         (car (org-export-unravel-code src-block)))
              (funcall          
               (org-ravel-get-ancestor-fun 'src-block info)
               src-block contents info)
              )))

### R inline-src-block transcoder

org-ravel-inline-src-block looks up org-ravel-inline-style-BACKEND, which
does the actual formatting. This function should not be modified by users.

    (defun org-ravel-inline-src-block (inline-src-block contents info)
      "Transcode an INLINE-SRC-BLOCK element from Org to backend markup.
    CONTENTS holds the contents of the item.  INFO is a plist holding
    contextual information.  Use default for parent backend except for R calls."
      (let ((lang (org-element-property :language inline-src-block))
            (ancestor-inline-src-block 
             (org-ravel-get-ancestor-fun 'inline-src-block info))
            (inline-style-fun 
             (intern (concat "org-ravel-inline-style-" 
                             (symbol-name 
                              (org-export-backend-name (plist-get info :back-end))))))
            )
        (if (and (string= lang "R") (fboundp inline-style-fun))
            (funcall inline-style-fun inline-src-block contents info)
          (funcall ancestor-inline-src-block inline-src-block contents info)
          )))

### advise for org-export-as

There is no hook at the beginning of `org-export-as`. So, to make this
work, `org-export-as` is 'advise'd to ad-activate advise for
org-babel-exp-do-export at the start, then ad-deactivate it at the
end.

1.  defadvice-org-export-as

         (defadvice org-export-as (around org-ravel-export-as-advice protect)
           "Activate advise for `org-babel-exp-do-export' in `org-export-as'.
         This enables preproceesing of R inline src blocks and src blocks
         by babel before parsing of the *.org buffer ."
           (if (fboundp  (intern (concat "org-ravel-chunk-style-" (symbol-name backend))))
               (progn
                 (add-hook 'org-export-before-parsing-hook 'org-ravel-strip-SRC-hookfun)
                 (add-hook 'org-export-before-parsing-hook 'org-ravel-strip-header-hookfun)
                 (ad-enable-advice 'org-babel-exp-do-export 'around 'org-ravel-exp-do-export)
                 (ad-activate 'org-babel-exp-do-export)
                 ad-do-it
                 (ad-disable-advice 'org-babel-exp-do-export 'around 'org-ravel-exp-do-export)
                 (ad-activate 'org-babel-exp-do-export)
                 (remove-hook 'org-export-before-parsing-hook 'org-ravel-strip-SRC-hookfun)
                 (remove-hook 'org-export-before-parsing-hook 'org-ravel-strip-header-hookfun))
             ad-do-it))
        
        (ad-activate 'org-export-as)

2.  org-ravel-strip-SRC-hookfun

    This hack works around the need to protect `src_R` calls from Babel until the
    export parser can see them. Also see advice for org-babel-exp-do-export
    
        
        (defun org-ravel-strip-SRC-hookfun ( backend )
          "Strip delimiters: ==SRC< and >SRC==. BACKEND is ignored."
          (save-excursion
            (goto-char (point-min))
            (while (re-search-forward "==SRC<\\(.*?\\)>SRC==" nil t)
              (replace-match "src_R\\1" nil nil))))

3.  org-ravel-strip-header-hookfun

    This hack works around the need to protect #+ATTR&#x2026; lines from Babel
    until the export parser can see them. Also see advice for org-babel-exp-do-export
    
        
        
        (defun org-ravel-strip-header-hookfun ( backend )
          "Strip #+header: ==<STRIP>==. BACKEND is ignored."
           (save-excursion
            (goto-char (point-min))
            (while (re-search-forward 
                    "^[         ]*#\\+headers?:[        ]*==<STRIP>==[ ]\\([^\n]*\\)$" 
                    nil t)
              (replace-match "\\1" nil nil))))

### advice for org-babel-exp-do-export

    (defadvice org-babel-exp-do-export  (around org-ravel-exp-do-export)
      "Wrap the ravel src block template around body and clip results."
      (let ((is-R (string= (nth 0 info) "R"))
            (is-inline (eq type 'inline))
            (export-val (or (cdr (assoc :exports (nth 2 info))) "code"))
            (noweb-yes (string= "yes" (cdr (assoc :noweb (nth 2 info))))))
        (if is-R
            ;; pass src block to the parser
            (setq ad-return-value
                  (if is-inline
                      ;; delimit src_R[]{} inside `==SRC<' and `>SRC=='
                      ;; it will be stripped just before parsing
                      ;; insert `:ravel' values, if any
                      (flet 
                       ((org-babel-examplize-region
                         (x y z)
                         (insert (format "==SRC<%s>SRC==" 
                                         (prog1 (buffer-substring x y)
                                           (delete-region x y)))))
                        (org-babel-execute:R
                         (body params)
                         (let* ((ravelarg 
                                 (cdr (assoc :ravel (nth 2 info))))
                                (nth-one (nth 1 info))
                                (srcRresult
                                 (if ravelarg
                                     (format "[ %s ]{%s}" ravelarg nth-one)
                                   (format "{%s}"
                                           nth-one ))))
                           srcRresult)
                         ))
                       (let ((org-confirm-babel-evaluate nil))
                         ad-do-it))
                    ;; #+begin_src ... #+end_src block
                    ;; omit if results type none
                    (if (string= export-val "none")
                        ""
                      ;; prune out any in buffer results
                      (org-babel-remove-result info)
                      (let* ((headers (nth 2 info))
                             (ravelarg (cdr (assoc :ravel headers)))
                             (non-ravelargs
                              (mapconcat 
                               '(lambda (x) (format "%S %s" (car x) (cdr x)))
                               (assq-delete-all :ravel headers) " ")))
                        (format "%s%s#+BEGIN_SRC R \n%s\n#+END_SRC"
                                (if ravelarg
                                    (format "#+header: ==<STRIP>== #+ATTR_RAVEL: %s\n" ravelarg) "")
                                (if non-ravelargs
                                    (format "#+header: ==<STRIP>== #+ATTR_R-HEADERS: %s\n" 
                                            non-ravelargs) "")
                                (if noweb-yes
                                    (org-babel-expand-noweb-references
                                     info
                                     (org-babel-exp-get-export-buffer))
                                  (nth 1 info)))))))
          ;; not R so do default
          ad-do-it)))


### miscellaneous

1.  minimal exporter



2.  insert src block name as chunk

    This helps when you have a src block you wish to include in an Sweave
    style document. Typically it would be called from inside a src block
    to add a noweb chunk. It will prompt for the name of the chunk.
    
        (defun org-ravel-insert-src-block-name-as-chunk ()
          "insert src block name in << >>."
        (interactive)
         (let
             ((bname
               (org-icompleting-read
                "source-block name: " (org-babel-src-block-names) nil t)))
           (insert (format "<<%s>>" bname))))

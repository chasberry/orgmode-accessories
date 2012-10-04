
# Background



## browsing this file

`ravel.org` is an orgmode file. 

If you are browsing `github`, you
should probably look at the file `ravel-org.md` which is formatted for viewing
(and omits lots of details that are usually not of interest). 

If you have downloaded a copy, it is best to view it using orgmode

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


**Warning:** Once the new exporter is replaces the old exporter, the
naming convention `prefix-e-suffix` will drop the 'e-'. If you create scripts that
use functions or backend names with 'e-', you will need to drop that
once the new exporter goes live.

## Put ./contrib/lisp on your load path

This works for my setup:

    (add-to-list 'load-path "~/elisp/org-mode/contrib/lisp/")

## Load the new exporter files

`e-ravel.el` will take care of this step, so if you go to the next
step - **extract e-ravel.el and load it** , it should *just work*.

`org-element` and `org-export` are required and at least one of the
defined backends. Here I use `e-latex` and `e-html` (which require
those other files):

    (require 'org-e-latex)
    (require 'org-e-html)

## extract e-ravel.el and load it

These two lines should do the trick:

    (org-babel-tangle)
    (load-file "e-ravel.el")

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
reproducible research formats: `brew`, `Sweave`, and `knitr`.

It does this by exporting the file or subtree in a suitable format
(currently LaTeX or HTML), but with differences from the usual export
mechanism in which the source code (aka src blocks) are evaluated by
orgmode's Babel engine and (optionally) code and/or results are passed
to the exporter.

Before the document is parsed, Babel is run. However, R scr blocks are
not evaluated. Instead they are passed on to the exporter as is except
that when the `:noweb yes` header argument is given, the expanded src
block is created (by inserting the code from the blocks in the noweb
references) and passed to the exporter. R scr blocks that have the
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
of the '<% &hellip; %>' code delimiters.

### existing backends

Currently, backends are avaiable for 

-   **`e-latex-noweb`:** LaTeX Sweave or knitr documents

-   **`e-latex-brew`:** LaTeX brew documents

-   **`e-beamer-noweb`:** beamer Sweave or knitr slides

-   **`e-html-knitr`:** HTML knitr documents

A look at the `*.org` files in 3 should provide a quickstart.
A look (below) at the definitions of the style functions
for these backends should guide further devlopment. 

### using Babel header arguments in exported code chunks

Babel header arguments are parsed and (the alist of such arguments is)
made available to the `org-ravel-chunk-style-BACKEND` function as
`r-headers-attr`. This will eventually allow translation of some
native org-babel headers to exported chunk headers. For this to
happen, the chunk style of a backend (say `e-latex-newstyle`) such as this

    (defun org-ravel-chunk-style-e-latex-newstyle 
      (label ravel r-headers-attr src-code)
      ( ... ))

needs to inspect the alist of `r-headers-attr` and find those that can
be (re-)rendered and add the necessary arguments to the output string
in the header position along with the arguments provided by the
`ravel` argument.

### explicit specification of arguments in exported chunks

Arguments that need to be passed to exported code chunks can be placed
after a `:ravel` key in a `#+begin_src R` line. Or they can be given
in `#+ATTR_RAVEL:` lines immediately before the src block. 

Some care is needed. Arguments for some backends may conflict with
other backends. In future development, it might help to prefix
arguments with the name of their backend.

### new backends

It is fairly easy to add more backends. There are these
ingredients needed:

1.  chunk style function

2.  inline style function

3.  a call to setup up the template-alist

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
    
    <pre class="example">
    #+NAME: defineDerived
    #+begin_src emacs-lisp :tangle e-ravel.el
        
      (org-export-define-derived-backend BACKEND PARENT
        :translate-alist ((src-block . org-ravel-src-block)
                          (inline-src-block . org-ravel-inline-src-block)
                          ))
      
      
    #+end_src
    </pre>
    
    where 'BACKEND' is the name of the new backend and 'PARENT' is the
    name of the original backend, e.g. "e-latex".
    
    The derived backends will inherit their *family name* and append the
    chunk style to it, e.g. `e-latex-noweb` for latex files using the
    traditional noweb style chunks.

## e-latex-noweb backend

This backend produces an Sweave noweb style document.

### template-alist

    (org-export-define-derived-backend e-latex-noweb e-latex
      :translate-alist ((src-block . org-ravel-src-block)
                        (inline-src-block . org-ravel-inline-src-block)
                        ))

### chunk style

    (defun org-ravel-chunk-style-e-latex-noweb 
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

    (defun org-ravel-inline-style-e-latex-noweb 
      (inline-src-block contents info)
      "Traditional Sweave style Sexpr using the INLINE-SRC-BLOCK element.
    CONTENTS holds the contents of the item.  INFO is a
    plist holding contextual information."
      (format "\\Sexpr{ %s }" (org-element-property :value inline-src-block)))

## e-beamer-noweb backend

### template alist

Note here the :latex-class is forced to "beamer" by default - I hate
using LATEX<sub>CLASS</sub> in the file as it bleeds into subtrees that are not
being exported.

    (org-export-define-derived-backend e-beamer-noweb e-beamer
      :translate-alist ((src-block . org-ravel-src-block)
                        (inline-src-block . org-ravel-inline-src-block)
                        ))

### augment latex-classes

    (unless
        (assoc "beamer" org-e-latex-classes)
      (add-to-list 
       'org-e-latex-classes  
       (append (list
                "beamer"
                "\\documentclass[11pt]{beamer}")
               (cddr (assoc "article" org-e-latex-classes)))))

### chunk style

This adds "%" before the code chunk to defeat e-beamer's tinkering
with contents which would add a space before a chunk (and thereby
disarm Sweave/knitr's processing of it)

    (defun org-ravel-chunk-style-e-beamer-noweb
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

    (defalias 'org-ravel-inline-style-e-beamer-noweb 'org-ravel-inline-style-e-latex-noweb)

## e-latex-brew backend

### brew document format

The [brew](http://cran.r-project.org/web/packages/brew/index.html) R package use "<%" and "%>" to delimit code chunks. Placing an equals sign (\=') after the first delimiter will result in the value of the expression being printed in place of the code chunk. Placing a minus sign (&shy;') before the last delimiter will suppress the line break after the code chunk.

It is handy to be able to evaluate the code inside the delimiters during development and debugging using the `C-c C-c` key, but this can only be done on complete, syntactically correct R expressions. In orgmode, this can be achieved by letting the delimiters live outside of the code chunk as an extra argument. Here is an example


<pre class="example">
#+begin_src R :ravel &lt;% code %&gt; 
  load("my-data.RData")
#+end_src

#+RESULTS:


#+begin_src R :ravel &lt;%= code %&gt;
  cat( print( ls() ), sep="\n")
#+end_src
</pre>

The code in each chunk can be executed via `C-c C-c`. On export, it is wrapped in the delimiters. The text `code` is deleted. (In fact, it need not be there at all.)

One of the nifty features of `brew` is that the code chunks do not need to be complete expressions. Thus, one can use

    The alphabet:
    <% for (i in letters) { %>
    <%= c( i, toupper(i) ) %>
    <% } %> 

to print the letters of the alphabet. In orgmode, the exporter becomes confused by code chunks like `for (i in letters) {`. Allowance for this idiom is made by placing the opening or closing curly brace just before the last delimiter (\[-]%>') like this `<% } -%>`. The curly brace will appear after the code (if any) in the chunk after export.

### template-alist

    (org-export-define-derived-backend e-latex-brew e-latex
       :translate-alist ((src-block . org-ravel-src-block)
                         (inline-src-block . org-ravel-inline-src-block)
                         ))  

### chunk style

    (defun org-ravel-chunk-style-e-latex-brew 
      (label ravel r-headers-attr src-code)
      "Default chunk style for brew style.
    LABEL is the chunk name,RAVEL is the collection of ravel args as a
    string,R-HEADERS-ATTR is the collection of headers from Babel as
    a string parseable by `org-babel-parse-header-arguments',SRC-CODE
    is the code from the block."
        (format (org-ravel-format-brew-spec ravel) src-code))

### inline style

    (defun org-ravel-inline-style-e-latex-brew 
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

<pre class="example">
format-brew-spec
</pre>

## e-html-knitr backend

This produces an html style document as supported by [knitr.](http://yihui.name/knitr/) 

### template-alist

    (org-export-define-derived-backend e-html-knitr e-html
      :translate-alist ((src-block . org-ravel-src-block)
                        (inline-src-block . org-ravel-inline-src-block)
                        ))

### chunk style

    (defun org-ravel-chunk-style-e-html-knitr 
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

    (defun org-ravel-inline-style-e-html-knitr 
      (inline-src-block contents info)
      "Traditional Sweave style Sexpr using the INLINE-SRC-BLOCK element.
    CONTENTS holds the contents of the item.  INFO is a
    plist holding contextual information."
      (format "<!--rinline %s -->" (org-element-property :value inline-src-block)))


## support functions

These functions should not be changed for individual backends.

### src block ancestors

Non-R src blocks should use the src-block and inline-src-block functions of
the parent (e.g. e-latex). This function helps to find them.

      (defun org-ravel-get-ancestor-fun (funkey &optional info)
    "Ancestral definition of function.
    Find  FUNKEY function in the `:translate-alist' property of INFO."
        (let ((anfun
               (cdr (assoc funkey (reverse (plist-get  info :translate-alist)))))
              )
          anfun))

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
                 (chunk-style-fun (intern (concat "org-ravel-chunk-style-" 
                                                  (symbol-name backend)))))
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
                (ancestor-inline-src-block (org-ravel-get-ancestor-fun 'inline-src-block))
                (inline-style-fun (intern (concat "org-ravel-inline-style-" 
                                                  (symbol-name backend))))
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
                (add-hook 'org-export-before-parsing-hook '
                          org-ravel-strip-SRC-hookfun)
                (ad-activate 'org-babel-exp-do-export)
                ad-do-it
                (remove-hook 'org-export-before-parsing-hook ' org-ravel-strip-SRC-hookfun)
                (ad-deactivate  'org-babel-exp-do-export)
                )
            ad-do-it))
        
        (ad-activate 'org-export-as)

2.  org-ravel-strip-SRC-hookfun

    This hack works around the need to protect `src_R` calls until the
    export parser can see them.
    
        (defun org-ravel-strip-SRC-hookfun ( backend )
          "Strip delimiters: ==SRC< and >SRC==. BACKEND is ignored."
            (while (re-search-forward "==SRC<\\(.*?\\)>SRC==" nil t)
              (replace-match "\\1" nil nil)))

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
                                     (format "src_R[%s]{%s}" ravelarg nth-one)
                                   (format "src_R{%s}"
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
                                    (format "#+ATTR_RAVEL: %s\n" ravelarg) "")
                                (if non-ravelargs
                                    (format "#+ATTR_R-HEADERS: %s\n" 
                                            non-ravelargs) "")
                                (if noweb-yes
                                    (org-babel-expand-noweb-references
                                     info
                                     (find-file-noselect org-current-export-file)) 
                                  (nth 1 info)))))))
          ;; not R so do default
          ad-do-it)))


### miscellaneous

1.  export as latex (sub-)document

    This function is helpful for making *child* documents in `knitr` or
    making documents that omit all the packages inserted by the exporter,
    which Sweave or knitr may prefer.
    
    The flet'ed `org-e-latex-template` is a subset of the original
    version.
    
        (defun org-ravel-export-e-latex-noweb (outfile &optional subtree only-contents)
          "Export a document to OUTFILE.  If SUBTREE is non-nil, just
        export the subtree. Use article format.
        
        ONLY-CONTENTS is treated as follows 
                nil - create a complete document as per e-latex, 
                t or 1 -  copy the contents without an enclosing 
                      document environment,
                2-6 - like t, but treat highest level heading as subsection, 
                      subsubsection, paragraph, subparagraph, or enumerate,
               \"Rnw\" - create an article documentclass with only hyperref as the 
                      default package, 
                any other string is inserted as a custom header."
          (cond
           ;; t - contents and minimal comments only 
           ((member only-contents '(t 1 2 3 4 5 6))
            (flet 
                ((org-e-latex-template 
                  (contents info)
                  (concat
                   ;; Time-stamp.
                   (and (plist-get info :time-stamp-file)
                        (format-time-string "%% Created %Y-%m-%d %a %H:%M\n"))
                   ;; Document's body.
                   contents
                   ;; Creator.
                   (let ((creator-info (plist-get info :with-creator)))
                     (cond
                      ((not creator-info) "")
                      ((eq creator-info 'comment)
                       (format "%% %s\n" (plist-get info :creator)))
                      (t (concat (plist-get info :creator) "\n")))))))
              (let* ((nsection (if (eq only-contents t) 1 only-contents))
                     (cur-latex-class (assoc "article" org-e-latex-classes))
                     (org-e-latex-classes 
                      (list (append
                       (butlast cur-latex-class 5)
                       (last cur-latex-class (- 6 nsection)))))) 
                (org-export-to-file 'e-latex-noweb outfile subtree))))
            ;; "Rnw" or string to use as custom header
           ((or 
             (and (string= "Rnw" only-contents) 
                  (setq only-contents 
                        "\\documentclass{article}\n[NO-DEFAULT-PACKAGES]\n\\usepackage{hyperref}"))
             only-contents)
            (let ((org-e-latex-classes 
                   (list
                    (append (list
                             "article"
                             only-contents)
                            (cddr (assoc "article" org-e-latex-classes))))))
              (org-export-to-file 'e-latex-noweb outfile subtree)))
           ;; nil - default export
           ((not only-contents)
            (org-export-to-file 'e-latex-noweb outfile subtree))))

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

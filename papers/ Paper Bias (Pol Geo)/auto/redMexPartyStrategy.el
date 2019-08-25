(TeX-add-style-hook "redMexPartyStrategy"
 (lambda ()
    (LaTeX-add-bibliographies
     "../bib/strategy")
    (LaTeX-add-labels
     "T:counterprops"
     "F:propsAndCost")
    (TeX-add-symbols
     "mc")
    (TeX-run-style-hooks
     "arydshln"
     "courier"
     "helvet"
     "scaled=.90"
     "mathptmx"
     "natbib"
     "sort"
     "longnamesfirst"
     "tikz"
     "hyperref"
     "hidelinks"
     "graphicx"
     "pdftex"
     "url"
     "amssymb"
     "amsmath"
     "fontenc"
     "T1"
     "setspace"
     ""
     "geometry"
     "bottom=1in"
     "top=1in"
     "left=1.25in"
     "right=1.25in"
     "letterpaper"
     "latex2e"
     "art12"
     "article"
     "12pt"
     "letter")))

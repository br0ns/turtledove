(TeX-add-style-hook "synopsis"
 (lambda ()
    (LaTeX-add-bibliographies)
    (TeX-add-symbols
     "EDSL")
    (TeX-run-style-hooks
     "mathdesign"
     "garamond"
     "hyperref"
     "fixme"
     "draft"
     "pdflscape"
     "colortbl"
     "babel"
     "english"
     "inputenc"
     "utf8"
     "fontenc"
     "T1"
     "latex2e"
     "memoir10"
     "memoir"
     "a4paper"
     "oneside")))


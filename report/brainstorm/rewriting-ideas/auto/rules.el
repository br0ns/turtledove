(TeX-add-style-hook "rules"
 (lambda ()
    (TeX-add-symbols
     "fun"
     "val"
     "flet"
     "fin"
     "fend"
     "map"
     "foldr"
     "foldl"
     "fif"
     "fthen"
     "felse"
     "true"
     "false"
     "fnot"
     "andalso"
     "oreelse"
     "fref"
     "Fref")
    (TeX-run-style-hooks
     "semantic"
     "mathenv"
     "mdwtab"
     "tikz"
     "ulem"
     "listings"
     "amssymb"
     "stmaryrd"
     "graphicx"
     "subfig"
     "caption"
     "hyperref"
     "fixme"
     "pdflscape"
     "fancyref"
     "babel"
     "british"
     "inputenc"
     "utf8"
     "fontenc"
     "T1"
     "latex2e"
     "memoir10"
     "memoir"
     "a4paper"
     "oneside"
     "final")))


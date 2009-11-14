;print "[Yeah baby!]\n";

val file = SourceText.fromFile "parsers/SourceText.sml"
;print (SourceText.getSource file 40 522 ^ "\n");

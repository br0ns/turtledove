open DataTypes
%%
%name Pi
%term CARET | DVBAR | EOF | EQUALS
    | IDE        of string
    | ILLCH | INPUT | LPAR | NEW | OUTPUT | RPAR
%nonterm abs      of A          | begin    of Pi
       | procList of Proc list | parallel of Proc list
       | pat      of Pat        | path     of Path
       | pi       of Proc list | proc      of Proc
       | value    of V
%pos int
%eop EOF
%noshift EOF
%nonassoc DVBAR EOF EQUALS ILLCH INPUT
          LPAR NEW OUTPUT RPAR
%nodefault
%verbose
%keyword NEW
%arg (fileName) : string

%%
begin: procList         ((Pi procList))
abs: pat EQUALS proc    ((A (pat,proc)))
procList: proc procList ((proc::procList))
|                       ([])
parallel:
  proc DVBAR parallel   ((proc::parallel))
| proc RPAR             ([proc])
| RPAR                  ([])
pat: path               ((Pat path))
path: IDE               ((Name (IDE,fileName,
                                IDEleft,IDEright)))
proc:
  NEW path proc         ((New (path,proc)))
| path OUTPUT value     ((Output (path,value)))
| path INPUT abs        ((Input (path,abs)))
| LPAR parallel         ((Parallel parallel))
value: path             ((V path))

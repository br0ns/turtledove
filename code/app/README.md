Applications 
============

This is a list of all the applications that have been implemented using
Turtledove. 


All applications ought to have a list of predefined test arguments for easy
execution. TO see the list run `make tests` in the respective application
directory and afterwards run `make XXX` for `XXX` being anything from returned
list of tests.


# Todo

Usage: `Todo Project.mlb`

Takes one argument, an mlb file, and walks through all referenced sml files for
comments starting with "TODO" and prints them to the console with information of
where they are located (file and location in file).


# Size

Usage: `Size Project.mlb`

Takes one argument, an mlb file, and walks through all referenced sml files and
counts their size and number of lines.

Also includes any Makefile located in directories containing referenced sml
files.

If lex/yacc files are referenced, then the .lex and .yacc files are counted
instead (i.e., not any of the auto generated code .lex.sml and yacc.sml)

The gathered size and lines are printed to the console

The size tool don't follow referenced paths that are above itself (the mlb file
that was given as argument), thus even though all mlb files (normally) reference
the standard library it will not be included in the count


# Normalform

Usage: `NormalForm [arguments] Project.mlb File.sml`

Takes two arguments, an mlb file and a sml files (the sml files must be
referenced inside the mlb file).

The referenced sml file will be normalised and the result (including information
on how it got normalised) is printed to the console.

<table>
<tr>
  <td><strong>Arguments</strong></td>
  <td><strong>Description</strong></td>  
</tr>
<tr>
  <td>--no-column</td>
  <td>The original and the result is Normally printed besides each other in two
  column. If this argument is given, then they will be printer after each
  other. This is used when the output is included in the report.</td>  
</tr>
<tr>
  <td>--verbose</td>
  <td>Will print verbose information to the console. This includes information
  from the normalisation process</td>  
</tr>
<tr>
  <td>-v</td>
  <td>Same as --verbose</td>  
</tr>
<table>


# Source explorer

Usage: `SourceExplorer Project.mlb`

Takes one argument, an mlb file, and prints "tree structure" to the console that
can be navigated through.

Navigation down the tree can be done by inputting the number of the desired node
to navigate into.

Other navigation keys are:
<table>
<tr>
  <td><strong>Key</strong></td>
  <td><strong>Meaning</strong></td>
  <td><strong>Description</strong></td>
</tr>
<tr>
  <td>u</td>
  <td>Up</td>
  <td>Navigate up to parent node.</td>
</tr>
<tr>
  <td>s</td>
  <td>Show</td>
  <td>Show the current node and any level of child nodes.</td>
</tr>
<tr>
  <td>r</td>
  <td>Right</td>
  <td>Show environment before the node.</td>
</tr>
<tr>
  <td>l</td>
  <td>Left</td>
  <td>Show the environment that the current node introduces
      I.E. left+right = new environment after node</td>
</tr>
<tr>
  <td>Ent</td>
  <td>Enter</td>

  <td>When the current node is an sml file, enter will go into the
      sml file and the exploration can continue.</td>
</tr>
<tr>
  <td>q</td>
  <td>Quit</td>

  <td>If entered into an sml file, quit will walk out of the file and back to
      the parent file including/referencing the current.  Up has the same effect
      as quit if the current node is the top most node in the current file.</td>
</tr>
</table>

# Rewrite

This is the currently available rewriting applications

## Map

Usage: `RewriteMap [arguments] Project.mlb File.sml`

Takes two arguments, an mlb file and a sml files (the sml files must be
referenced inside the mlb file).

The referenced sml file will be normalised and then if possible rewritten
to use map..

<table>
<tr>
  <td><strong>Arguments</strong></td>
  <td><strong>Description</strong></td>  
</tr>
<tr>
  <td>--no-column</td>
  <td>The original and the result is Normally printed besides each other in two
  column. If this argument is given, then they will be printer after each
  other. This is used when the output is included in the report.</td>  
</tr>
<tr>
  <td>--verbose</td>
  <td>Will print verbose information to the console. This includes information
  from the normalisation process</td>  
</tr>
<tr>
  <td>-v</td>
  <td>Same as --verbose</td>  
</tr>
<table>

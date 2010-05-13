
exception Quit;
exception Reloop of ProjectManager.t

val info = "\nFollowing are valid actions: \n"
         ^ "q   : Quit.\n" 
         ^ "save: Saves the project.\n"
         ^ "pgn : Print the project group name.\n"
         ^ "ls  : Print the project in a structured list.\n"
         ^ "mlb : Print the resulting MLB Description of the project.\n"
         ^ "\n"
         ^ "lsf : List all files in the project.\n"
         ^ "adf : Add new file to project.\n"
         ^ "rmf : Remove file from project.\n"
         ^ "rnf : Rename file in project.\n"
         ^ "\n"
         ^ "lsg : List all groups in the project.\n"
         ^ "adg : Add group to project.\n"
         ^ "rmg : Remove group from project.\n"
         ^ "rng : Rename group in project.\n"
         ^ "\n"
         ^ "lsd : List all dependencies in the project.\n"
         ^ "add : Add a dependency to the project.\n"
         ^ "rmd : remove a dependency from the project.\n"
         ^ "\n"
         ^ "lse : List all group- and filenames that are exposed.\n"
         ^ "ade : Add a group- or filename to be exposed.\n"
         ^ "rme : Remove a group- or filename from the list of exposed.\n"

fun input () = 
    let
      val line = valOf (TextIO.inputLine TextIO.stdIn)
    in
      String.substring (line, 0, ((String.size line) - 1))
    end

fun printParrentGroups t = (print "\nPossible Groups and Files: \n";
                            print (ProjectManager.listFilesAndGroups t);
                            print "\n")

fun printStringOrderedSet set = print (StringOrderedSet.toString (String.toString) set)

fun printDictionary dict = print (Dictionary.toString (String.toString) (StringOrderedSet.toString (String.toString)) dict);
                           
fun printDependencies t =
    (print "Project contains the following dependencies: \n";
     printDictionary (ProjectManager.getDependencies t);
     print "\n")
    
fun action t = 
    let
      val line = (print "Action: ";
                  input())
    in
      (case line of 
         "q" => raise Quit
       | "save" => saveProject t
       | "pgn" => projectGroupName t
       | "ls" => ls t

       | "lsf" => lsFiles t
       | "adf" => addFile t
       | "rmf" => removeFile t
       | "rnf" => renameFile t

       | "lsg" => lsGroups t
       | "adg" => addGroup t
       | "rmg" => removeGroup t                
       | "rng" => renameGroup t

       | "lsd" => lsDependencies t
       | "add" => addDependency t
       | "rmd" => removeDependency t

       | "lse" => lsExposes t
       | "ade" => addExpose t
       | "rme" => removeExpose t

       | "mlb" => (print (ProjectManager.MLB.description t);
                   print "\n";
                   action t)
                  
       | x => (print info; action t)) 
    end

and saveProject t =
    (print "Saving project...\n";
     ProjectManager.saveProject t;
     print "Save at: ";
     print (ProjectManager.getProjectPath t);
     action t)

and projectGroupName t = 
    (print "The projects name is:";
     print (ProjectManager.getProjectGroupNameStr t);
     print "\n\n";
     action t)

and lsFiles t = (printStringOrderedSet (ProjectManager.getFileNames t);
                 print "\n\n";
                 action t)

and addFile t =
    let
      val _ = printParrentGroups t
      val file = 
          (print "New filename: ";
           input())
      val group = (print "Add to parrent group: ";
                   input())
    in
      ProjectManager.addFile t group file
    end

and removeFile t =
    let
      val _ = printParrentGroups t
      val file = 
          (print "Remove filename: ";
           input())
      val group = (print "Remove from parrent group: ";
                   input())
    in
      ProjectManager.removeFile t group file
    end

and renameFile t =
    let
      val _ = printParrentGroups t
      val oldFile = 
          (print "Old filename: ";
           input())
      val newFile = 
          (print "New filename: ";
           input())
      val group = (print "Rename in parrent group: ";
                   input())
    in
      ProjectManager.renameFile t group oldFile newFile
    end

and lsGroups t = (printStringOrderedSet (ProjectManager.getGroupNames t);
                  print "\n\n";
                  action t)

and addGroup t =
    let
      val _ = printParrentGroups t
      val newGroup = 
          (print "New group name: ";
           input())
      val group = (print "Add to parrent group: ";
                   input())
    in
      ProjectManager.addGroup t group newGroup
    end

and removeGroup t =
    let
      val _ = printParrentGroups t
      val newGroup = 
          (print "Remove group name: ";
           input())
      val group = (print "Remove from parrent group: ";
                   input())
    in
      ProjectManager.removeGroup t group newGroup
    end

and renameGroup t =
    let
      val _ = printParrentGroups t
      val oldGroup = 
          (print "Old group name: ";
           input())
      val newGroup = 
          (print "New group name: ";
           input())
      val group = (print "Remove from parrent group: ";
                   input())
    in
      ProjectManager.renameGroup t group oldGroup newGroup
    end

and ls t = (printParrentGroups t; action t)
           
and lsDependencies t = (printDependencies t; action t)

and addDependency t =
    let
      val _ = (printParrentGroups t;
               printDependencies t)

      val depFrom = (print "Dependency from: ";
                     input())

      val depTo = (print "Dependency to: ";
                   input())               
    in
      ProjectManager.addDependency t depFrom depTo
    end

and removeDependency t = 
    let
      val _ = (printParrentGroups t;
               printDependencies t)

      val depFrom = (print "Dependency from: ";
                     input())

      val depTo = (print "Dependency to: ";
                   input())               
    in
      ProjectManager.removeDependency t depFrom depTo
    end
    
and lsExposes t = (print "Groups Expose the following\n";
                   printDictionary (ProjectManager.getExposes t);
                   print "\n\n";
                   action t)
                  

and addExpose t =
    let
      val _ = (printParrentGroups t;
               printDictionary (ProjectManager.getExposes t);
               print "\n")

      val expose = 
          (print "Group- or filename to expose: ";
           input())

      val group = (print "Expose in group: ";
                   input())
    in
      ProjectManager.addExpose t group expose
    end

and removeExpose t =
    let
      val _ = (printParrentGroups t;
               printDictionary (ProjectManager.getExposes t);
               print "\n")

      val expose = 
          (print "Group- or filename to un-expose: ";
           input())

      val group = (print "Remove from group: ";
                   input())
    in
      ProjectManager.removeExpose t group expose
    end

fun loop () =
    let 
      fun loop' t =  
          let                      
            val _ = print "\n\n"
                    
            val t' = action t
                handle Fail s => (print "\n";
                                  print "!! Exception:\n";
                                  print ("!! " ^s ^ "\n");
                                  print "\n";
                                  raise Reloop t) 
                                 
            val _ = (print "\n";
                     print (ProjectManager.projectToString t');
                     print "\n\n==============================================\n")                     
          in
            loop' t'
          end                                       
              
      fun loop'' t = loop' t handle Reloop t' => loop'' t'
    in
      ( print "For help type '?'\n\n";
        print "Current possibilities are:\n";
        print "init : Initialises a new project.\n";
        print "open : Opens a new project from file.\n";
        loop'' (print "Action: ";
                case input() of
                  "init" => (print "Name of new project: ";
                             ProjectManager.init (input()) (OS.FileSys.tmpName ()))
                | "open" => (print "Path of projectfile to open: ";
                             ProjectManager.openProject (input ()))
                | x => raise Fail "Not a valid action! Quitting...." ))
    end
    handle Quit => print "Quitting.\n"


;loop();

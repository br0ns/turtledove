
exception Quit;
exception Reloop of ProjectManager.t

val info = "\nFollowing are valid actions: \n"
         ^ "q   : Quit.\n"         
         ^ "pgn : Print the project group name.\n"
         ^ "ls  : Print the project in a structured list.\n"
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

fun input() = 
    let
      val line = valOf (TextIO.inputLine TextIO.stdIn)
    in
      String.substring (line, 0, ((String.size line) - 1))
    end

fun printParrentGroups t = (print "\nPossible Groups and Files: \n";
                            print (ProjectManager.listFilesAndGroups t);
                            print "\n")

fun printStringOrderedSet set = print (StringOrderedSet.toString (String.toString) set)
                           
fun printDependencies t =
    let
      val dependencies = ProjectManager.getDependencies t
    in
      (print "Project contains the following dependencies: \n";
       print (Dictionary.toString (String.toString) (StringOrderedSet.toString (String.toString)) dependencies);
       print "\n")

    end
                           
fun action t = 
    let
      val line = (print "Action: ";
                  input())
    in
      (case line of 
         "q" => raise Quit
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

       | x => (print info; action t)) 
    end

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
      val _ = printDependencies t

      val depFrom = (print "Dependency from: ";
                     input())

      val depTo = (print "Dependency to: ";
                   input())               
    in
      ProjectManager.removeDependency t depFrom depTo
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
          
      val projectName = (print "name of new project: ";
                         input())
      val t = (print "Creating new project... \n";
               ProjectManager.init projectName (OS.FileSys.tmpName ()))
              
      val _ = print (ProjectManager.projectToString t)
              
      fun loop'' t = loop' t handle Reloop t' => loop'' t'
    in
      ( print "For help type '?'\n";
        loop'' t)
    end
    handle Quit => print "Quitting.\n"


;loop();

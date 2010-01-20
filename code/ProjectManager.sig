signature ProjectManager = 
sig

  type t
       
  (* project name, project path  *)     
  val init : string -> string -> t
                                 


  (* open a project at the specified path *)
  val openProject : string -> t
  
  (* save the currently open project *)
  val saveProject : t -> unit

                      

  val getFileNames : t -> StringOrderedSet.t

  (* filename, parrent group name, project, resulting project  *)
  val addFile : string -> string -> t -> t

  (* filename, parrent group name, project, resulting project  *)
  val removeFile : string -> string -> t -> t

  (* filename, old parrent group name, new parrent group name, 
     project, resulting project *) 
  val moveFile : string -> string -> string -> t -> t



  val getGroupNames : t -> StringOrderedSet.t

  (* group name, add to group, project, resulting project *)
  val addGroup : string -> string -> t -> t

  (* group name, parrent group name, main project, resulting main project *)
  val removeGroup : string -> string -> t -> t

  (* group name, old parrent group name, new parrent group name,
     project, resulting project *)
  val moveGroup : string -> string -> string -> t -> t



  (* main project, dictionary with project name as key and StringOrderedSet of
     dependencys as values *)
  val getDependencys: t -> StringOrderedSet.t Dictionary.t

  (* dependency from, dependency to, main project, resulting main project *)
  val addDependency : string -> string -> t -> t


  val getProperties : t -> JSON.t Dictionary.t
  val setProperty : t -> (string * JSON.t) -> t


  val toString : t -> string
  val show : t -> Report.t

end

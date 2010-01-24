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

  (* project, parrent group name, filename, resulting project  *)
  val addFile : t -> string -> string -> t

  (* project, parrent group name, filename, resulting project  *)
  val removeFile : t -> string -> string -> t

  (* project, old parrent group name, new parrent group name, filename,
     resulting project *)
  val moveFile : t -> string -> string -> string ->  t

  (* project, parrent group name, old filename, new filename,
     resulting project *)
  val renameFile: t -> string -> string -> string -> t


  val getProjectGroupName : t -> string

  val getGroupNames : t -> StringOrderedSet.t

  (* project, parrent group name, new group name, resulting project *)
  val addGroup : t -> string -> string -> t

  (* main project, parrent group name, group name, resulting main project *)
  val removeGroup : t -> string -> string -> t
etPrope
  (* project, old parent group name, new parrent group name, group name,
     resulting project *)
  val moveGroup : t -> string -> string -> string -> t

  (* project, parent group name, old group name, new group name,
     resulting project *)
  val renameGroup : t -> string -> string -> string -> t


  (* project, dictionary with project name as key and StringOrderedSet of
     dependencys as values *)
  val getDependencies: t -> StringOrderedSet.t Dictionary.t

  (* project, dependency from, dependency to, resulting main project *)
  val addDependency : t -> string -> string -> t

  (* project, dependency from, dependency to, resulting main project *)
  val removeDependency : t -> string -> string -> t


  (* if property exists, then overwrite it *)
  val setProperty : t -> (string * JSON.t) -> t

  val getProperties : t -> JSON.t Dictionary.t


  val toString : t -> string
  val show : t -> Report.t

end

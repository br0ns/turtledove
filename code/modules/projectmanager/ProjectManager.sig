signature ProjectManager = 
sig

  type t
       
  structure MLB :
            sig              

              val description : t -> string                                    
                                     
            end
       
  (* project name, project path  *)     
  val init : string -> string -> t
     
                            
  (* open a project at the specified path *)
  val openProject : string -> t
  
  (* save the currently open project. Returns the path *)
  val saveProject : t -> unit
                         
  (* Returns the path of the project *)
  val getProjectPath : t -> string                  

  val getFileNames : t -> StringSet.t

  (* project, parrent group name, filename, resulting project  *)
  val addFile : t -> string -> string -> t

  (* project, parrent group name, filename, resulting project  *)
  val removeFile : t -> string -> string -> t

  (* project, parrent group name, old filename, new filename,
     resulting project *)
  val renameFile: t -> string -> string -> string -> t



  val getProjectGroupNameStr : t -> string

  val getGroupNames : t -> StringSet.t

  (* project, parrent group name, new group name, resulting project *)
  val addGroup : t -> string -> string -> t

  (* main project, parrent group name, group name, resulting project *)
  val removeGroup : t -> string -> string -> t

  (* project, parent group name, old group name, new group name,
     resulting project *)
  val renameGroup : t -> string -> string -> string -> t


  (* project, dictionary with project name as key and StringSet of
     dependencys as values *)
  val getDependencies: t -> StringSet.t Dictionary.t

  (* project, dependency from, dependency to, resulting project *)
  val addDependency : t -> string -> string -> t

  (* project, dependency from, dependency to, resulting project *)
  val removeDependency : t -> string -> string -> t



  val getExposes : t -> StringSet.t Dictionary.t
                                                  
  (* project, parrent group that exposes this file/group, group/file that are to
  be exposed, resulting project *)
  (* TODO: Since a particular group or file can only exist at one place the group name is not needed here. In other words: val addExpose : t -> string -> t *)
  val addExpose : t -> string -> string -> t

  (* project, parrent group that exposes this file/group, group/file that are to
  be un-exposed, resulting project *)
  val removeExpose : t -> string -> string -> t



  (* if property exists, then overwrite it *)
  val setProperty : t -> (string * JSON.t) -> t

  val getProperties : t -> JSON.t Dictionary.t



  val listFilesAndGroups : t -> string
  val projectToString : t -> string
  val toString : t -> string
  val show : t -> Report.t

end

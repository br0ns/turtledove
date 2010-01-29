structure ProjectManager :> ProjectManager =
struct

type t = {path: Path.t, project : JSON.t}

fun unimp _ = raise Fail "ProjectManager: Unimplemented"

fun die s = raise Fail ("ProjectManager: " ^ s)


structure Generate =
struct 

  fun dependencyStr depFrom "" = "{\"Name\" : \"" ^ depFrom ^ "\", \"Depends\" : [  ]}"
    | dependencyStr depFrom depTo = "{\"Name\" : \"" ^ depFrom ^ "\", \"Depends\" : [ \"" ^ depTo ^ "\" ]}"

  fun dependency depFrom depTo = JSON.read (dependencyStr depFrom depTo)

  fun emptyDependencyStr depFrom = dependencyStr depFrom ""
  val emptyDependency = JSON.read o emptyDependencyStr
                                                                                                          

  val emptyValueStr = "{ \"Exposes\": [], \"Nodes\": [] }"
  val emptyValue = JSON.read emptyValueStr
  
  fun emptyGroupStr name = "{\"Name\": \"" ^ name ^ "\", \"Value\": " ^ emptyValueStr  ^ "}"
  val emptyGroup = JSON.read o emptyGroupStr

  fun emptyProjectStr name = "{ \"Properties\" : { }, \"Dependencies\" : [  ], \"ProjectNode\" : " ^ (emptyGroupStr name)   ^ "}"
  val emptyProject = JSON.read o emptyProjectStr
                        
end


fun init name projectPath = 
    let
      val path = Path.new projectPath
      val project = Generate.emptyProject name
      (* openOut creates the file it it dosn't exist *)
      val fs = File.openOut path
    in
      (
       TextIO.output (fs, (JSON.show project));
       TextIO.closeOut fs;
       {path = path, project =  project}
      )
    end;                         
    
(* FIX: Validate the project instead of accepting it blindley *)
(* FIX: Peek to see if first char is "{" *)
(*      if not, or not a valid JSON then open it as "Single file" project *)    
fun openProject projectPath =
    let
      val path = Path.new projectPath
      val fs = File.openIn path
      val project = JSON.read (TextIO.inputAll fs)
    in
      (
       TextIO.closeIn fs;
       {path = path, project = project}
      )
    end;
    
fun saveProject ({path, project, ...}: t) = 
    let
      val fs = File.openOut path
    in      
      TextIO.output (fs, (JSON.show project)) before TextIO.closeOut fs
    end;

(* 
 * Helper functions to get fields out of the various JSON Objects that makes out a project.
 * As the project is validated when opened we know for sure that the dictionary contains the 
 * required fields we lookup.
 *)
fun getFieldFromJSONObject (JSON.Object dict) field = valOf (Dictionary.lookup dict field)
  | getFieldFromJSONObject x _ = die ("Expected a JSON Object to lookup in, but got: " ^ JSON.write x)

(* 
 * The maning convention "...FromXxx" could be argued to be stupid, but it somehow forces
 * one to think twice when using the function, that the passed JSON.Object is actually a 
 * "Value-Object" or "Node-Object". As there is no type safety in the different types of 
 * JSON.Object's abd the lookup would else failse miserably since getFieldFromJSONObject 
 * blindly assumes that field exists by doing "valOf"
 *)

fun getPropertiesFromProject  project  = getFieldFromJSONObject project "Properties";
fun getDependenciesFromProject project = getFieldFromJSONObject project "Dependencies";
fun getProjectNodeFromProject project  = getFieldFromJSONObject project "ProjectNode";

fun getNameFromNode  node = getFieldFromJSONObject node "Name";
fun getDependsFromNode  node = getFieldFromJSONObject node "Depends";
fun getValueFromNode node = getFieldFromJSONObject node "Value";              

fun getExposesFromValue value = getFieldFromJSONObject value "Exposes";
fun getNodesFromValue   value = getFieldFromJSONObject value "Nodes";


fun updateProjectInRecord {path, project} newProject = {path = path, project=newProject}

fun updateDict dict key value = Dictionary.update dict (key, value)
fun updateObject obj key value = JSON.Object (Dictionary.update (JSON.objectOf obj) (key, value))


  (* Searches through a group and it subgroups untill it findes a groups with
     the name of 'parrentGroupStr' and then applys the 'applyToNodesFun' on all
     the nodes on that parrent group. *)
  fun applyToParrentGroupNodes applyToNodesFun parrentGroupStr (node as JSON.Object _) =
      let
        val nameStr = JSON.stringOf (getNameFromNode node)                       
                      
        val value = getValueFromNode node
        val nodes = getNodesFromValue value

        val (modified, newNodes) =
            (if nameStr = parrentGroupStr then        
               let
                 val (modified, newNodes) = applyToNodesFun nodes
               in
                 if modified then
                   (modified, newNodes)
                 else
                   die ("Parrent group ("^parrentGroupStr^") was found, but the 'apply' function didn't modify any of the nodes")
               end
             else (* this is not the parent group, check child groups. *)
               JSON.mapUntil (applyToParrentGroupNodes applyToNodesFun parrentGroupStr) nodes)
      in
        if modified then
          let
            val newValue = updateObject value "Nodes" newNodes
            val newNode = updateObject node "Value" newValue
          in (* modified true *)
            (modified, newNode)
          end
        else (* modified false *)
          (modified, node)
      end
    | applyToParrentGroupNodes _ _ x = (false, x)
      
 
  fun getFileNames ({project, ...}: t)  = 
      let                            
       fun extractFiles ((node as JSON.Object _), res) = JSON.fold extractFiles res ((getNodesFromValue o getValueFromNode) node)
         | extractFiles ((JSON.String s), res) = StringOrderedSet.insert res s
         | extractFiles (x,res) = die ("Only JSON.Object and JSON.String are allowed in the 'Nodes' list: " ^ JSON.write x)
      in
        extractFiles ((getProjectNodeFromProject project ), StringOrderedSet.empty)
      end;

      
  fun addFile (r as {project, ...}: t) parrentGroupStr filenameStr = 
      let
        val files = getFileNames r
        val _ = StringOrderedSet.member files filenameStr andalso die ("The file: '" ^ filenameStr ^  "' is already present in this project")

        fun addFileToNodes nodes = (true, JSON.cons(JSON.String filenameStr, nodes))
              

        val projectNode = getProjectNodeFromProject project
        val (modified, newProjectNode) = applyToParrentGroupNodes addFileToNodes parrentGroupStr projectNode
      in
        if modified then 
          updateProjectInRecord r (updateObject project "ProjectNode" newProjectNode)
        else
          die ("The file: '" ^ filenameStr ^  "' doesn't exist. The new file was not added")
      end

  fun removeFile (r as {project, ...}: t) parrentGroupStr filenameStr  =
      let

        fun isFileToBeRemoved (JSON.String s) = s = filenameStr
          | isFileToBeRemoved x = false

        val projectNode = getProjectNodeFromProject project
        val (modified, newProjectNode) = applyToParrentGroupNodes (JSON.filterUntil isFileToBeRemoved) parrentGroupStr projectNode
      in
        if modified then 
          updateProjectInRecord r (updateObject project "ProjectNode" newProjectNode)
        else
          die ("The file: '" ^ filenameStr ^  "' doesn't exist. The file was not removed")
      end

  fun renameFile (r as {project, ...}: t) parrentGroupStr oldFilenameStr newFilenameStr =
      let              
        fun locateFileToRename (str as JSON.String s) = 
            if s = oldFilenameStr then
              (true, JSON.String newFilenameStr)
            else
              (false, str)
          | locateFileToRename x = (false, x)
        
        val projectNode = getProjectNodeFromProject project
        val (modified, newProjectNode) = applyToParrentGroupNodes (JSON.mapUntil locateFileToRename) parrentGroupStr projectNode
      in
        if modified then 
          updateProjectInRecord r (updateObject project "ProjectNode" newProjectNode)
        else
          die ("The file: '" ^ oldFilenameStr ^  "' doesn't exist. The file was not renamed")
      end


  fun getProjectGroupName ({project, ...}: t) = JSON.stringOf (getNameFromNode (getProjectNodeFromProject project))

(* There might be a problem here. The structure of which groups belong to who is lost *)
  fun getGroupNames ({project, ...}: t) = 
      let
        fun parseNode ((node as JSON.Object _), res) =
            let          
              (* The returned field is a JSON.String so writ it to a ordinary string *)              
              val name = JSON.stringOf (getNameFromNode node)
            in
              JSON.fold parseNode (StringOrderedSet.insert res name) ((getNodesFromValue o getValueFromNode) node)
            end
          (* As group names are only located in the JSON Objects we skip anything else*)
          | parseNode (_,  res) = res

      in
        parseNode ((getProjectNodeFromProject project), StringOrderedSet.empty)
      end;


  fun addGroup (r as {project, ...}: t) parrentGroupStr newGroupStr = 
      let
        val groups = getGroupNames r
        val _ = StringOrderedSet.member groups newGroupStr andalso die ("The group: '" ^ newGroupStr ^  "' is already present in this project")

        fun addGroupToNodes nodes =
            let
              val newGroup = Generate.emptyGroup newGroupStr
              val newNodes = JSON.cons(newGroup, nodes)
            in
              (true, newNodes)
            end
            
        val projectNode = getProjectNodeFromProject project
        val (modified, newProjectNode) = applyToParrentGroupNodes addGroupToNodes parrentGroupStr projectNode
      in
        if modified then 
          updateProjectInRecord r (updateObject project "ProjectNode" newProjectNode)
        else
          die ("The group: '" ^ newGroupStr ^  "' doesn't exist. The new group was not added")
      end


  fun removeGroup (r as {project, ...}: t) parrentGroupStr removeGroup = 
      let
        
        fun isGroupToBeRemoved (node as JSON.Object _) = (JSON.stringOf (getNameFromNode node)) = removeGroup                                      
          | isGroupToBeRemoved x = false
                                      
        val projectNode = getProjectNodeFromProject project
        val (modified, newProjectNode) = applyToParrentGroupNodes (JSON.filterUntil isGroupToBeRemoved) parrentGroupStr projectNode
      in
        if modified then 
          updateProjectInRecord r (updateObject project "ProjectNode" newProjectNode)
        else
          die ("The group: '" ^ removeGroup ^  "' doesn't exist. The group was not removed")
      end     

  fun renameGroup (r as {project, ...}: t) parrentGroupStr oldGroupStr newGroupStr =
      let
        fun locateNodeToRename (node as JSON.Object dict) =
            let
              val nameStr = JSON.stringOf (getNameFromNode node)
            in
              if nameStr = oldGroupStr then
                (true, JSON.Object  (updateDict dict "Name" (JSON.String newGroupStr)))
              else
                (false, node)               
            end  
          | locateNodeToRename x = (false, x)

        val projectNode = getProjectNodeFromProject project
        val (modified, newProjectNode) = applyToParrentGroupNodes (JSON.mapUntil locateNodeToRename) parrentGroupStr projectNode
      in
        if modified then 
          updateProjectInRecord r (updateObject project "ProjectNode" newProjectNode)
        else
          die ("The group: '" ^ oldGroupStr  ^  "' doesn't exist. The new group was not renamed")        
      end
      
  fun getDependencies ({project, ...}: t) =
      let

        fun extractDepend ((JSON.String s), res) = StringOrderedSet.insert res s
          | extractDepend (x, _) = die ("Expected a JSON String as 'Depends' constraint, but got: " ^ JSON.write x)

        fun extractDependency ((node as JSON.Object dict), res) =
            let
              val nameStr = JSON.stringOf (getNameFromNode node)
              val depends = getDependsFromNode node
              val dep = Dictionary.lookup res nameStr
            in
              if isSome dep then
                Dictionary.update res (nameStr, JSON.fold extractDepend (valOf dep) depends)
              else
                Dictionary.update res (nameStr, JSON.fold extractDepend (StringOrderedSet.empty) depends)
            end
          | extractDependency (x, _) = die ("Expected a JSON Object as dependency to lookup in, but got: " ^ JSON.write x)

      in
        JSON.fold extractDependency Dictionary.empty (getDependenciesFromProject project)
      end;

  fun addDependency (r as {project, ...}: t) depFrom depTo =
      let

        

        val files = getFileNames r
        val groups = getGroupNames r

        val _ = (StringOrderedSet.member files depFrom orelse StringOrderedSet.member groups depFrom)
                andalso
                (StringOrderedSet.member files depTo orelse StringOrderedSet.member groups depTo)
                andalso die ("The dependency from '"^ depFrom ^"' to '"^ depTo ^"' is already in the dependency list")

        fun insertDepend dependsLst =
            let
              (* If the depTo exists in the list then die, else add it to the list.  *)
              val _ = (JSON.exists (fn x => (JSON.stringOf x) = depTo) dependsLst) andalso die ("The depends list already contains a dependency to '" ^ depTo )
            in
              JSON.cons(JSON.String depTo, dependsLst)
            end

        fun locateDependency (node as JSON.Object _) =
            if ((JSON.stringOf (getNameFromNode node)) = depFrom) then (* match, update the depends with the depTo *)
              let
                val depends = getDependsFromNode node
                val newDepends = insertDepend depends
                val newDependency = updateObject node "Depends"  newDepends
              in
                (true, newDependency)
              end                 
            else (* no match, keep going *)
              (false, node)
          | locateDependency x = die ("Expected a JSON Object as dependency to lookup in, but got: " ^ JSON.write x)
                                
        val dependencies = getDependenciesFromProject project

        val (modified, newDependencies) = JSON.mapUntil locateDependency dependencies
                                          
        val newDependencies = (if modified then (* match, we have update the list *)
                                 newDependencies
                               else (* No match, insert the new dependency *)
                                 JSON.cons((Generate.dependency depFrom depTo), newDependencies))
                                 
                                 
        val newProject = updateObject project "Dependencies" newDependencies
      in
        updateProjectInRecord r newProject
      end

  fun removeDependency (r as {project, ...}: t) depFrom depTo =
      let

        fun removeDepend depend = (JSON.stringOf depend) = depTo

        fun locateDependency (node as JSON.Object _) =
            if ((JSON.stringOf (getNameFromNode node)) = depFrom) then 
              (* match, remove the objec *)
              let
                val depends = getDependsFromNode node
                val (modified, newDepends) = JSON.filterUntil removeDepend depends
                val newDependency = if modified then 
                                      updateObject node "Depends" newDepends
                                    else
                                      die ("The dependency to '"^ depTo ^"' was not found in the list of dependencies from '"^ depFrom ^"'.")
              in
                (true, newDependency)
              end
            else (* no match, keep going *)
              (false, node)
          | locateDependency x = die ("Expected a JSON Object as dependency to lookup in, but got: " ^ JSON.write x)
        
        val dependencies = getDependenciesFromProject project
        val (modified, newDependencies) = JSON.mapUntil locateDependency dependencies

        val newProject = if modified then
                           updateObject project "Dependencies" newDependencies
                         else
                           die ("The dependency from '"^ depFrom  ^"' was not found in the list of dependencies.")
      in
        updateProjectInRecord r newProject
      end

  val setProperty = unimp;

  fun getProperties ({project, ...}: t) = JSON.objectOf (getPropertiesFromProject project)  

  fun listFilesAndGroups (r as {project, ...}: t)  =
      let

        (* pretty print group names with indent for each sub group *)
        fun listFilesAndGroups' indent ((node as JSON.Object _), res) = 
            let
              val nameStr = JSON.stringOf (getNameFromNode node)    
              val nodes = (getNodesFromValue o getValueFromNode) node
                          
              val res' = res ^ (indent ^ "G: '" ^ nameStr ^ "'\n")
            in
              JSON.fold (listFilesAndGroups' ("   " ^ indent)) res' nodes
            end
          | listFilesAndGroups' indent ((JSON.String s), res) = res ^ (indent ^ "F: '" ^ s ^ "'\n")
          | listFilesAndGroups' _ (x,res) = res (* ignore anything else *)

        val projectNode = getProjectNodeFromProject project
        val projectNameStr = JSON.stringOf (getNameFromNode projectNode)

        val projectNodes = (getNodesFromValue o getValueFromNode) projectNode
      in
        "Labels: (G)roup, (F)ile\n" ^
        "-------------------------------\n" ^
        (JSON.fold (listFilesAndGroups' "|- ") ("G: '" ^ projectNameStr ^ "'\n") projectNodes) ^
        "\n"
      end

  fun projectToString (r as {project,...}: t) = (JSON.show project) ^ "\n";

  fun toString (r as {project, path,...}: t) =
      "project file: " ^ Path.path path ^ "\n" ^
      "Project: \n" ^ JSON.show project;

        
  val show = unimp;


end

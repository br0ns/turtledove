(* TODO: Implement "Single file" mode. Remember no actions are allowed when
         opened this waya. *)


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

fun getProjectPath ({path, ...}: t) = Path.toString path

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
fun updateObject obj key value = JSON.Object (Dictionary.update (JSON.dictionaryOf obj) (key, value))


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

  fun locateParrentGroup parrentGroupStr node =
      let       
        exception NotFound

        fun isNodeNamed node nameStr = (JSON.stringOf (getNameFromNode node)) = nameStr
                                       
        fun locateParrentGroupInNode node =
            if isNodeNamed node parrentGroupStr then
              node     
            else
               locateParrentGroupInNodes (JSON.listOf ((getNodesFromValue o getValueFromNode) node))
              
              
        and locateParrentGroupInNodes ((x as JSON.Object _) :: xs) = (locateParrentGroupInNode x
                                                                      handle NotFound => locateParrentGroupInNodes xs  )            
          | locateParrentGroupInNodes (_ :: xs) = locateParrentGroupInNodes xs 
          | locateParrentGroupInNodes [] = raise NotFound
                                                 
      in
        locateParrentGroupInNode node handle NotFound => die ("The group '" ^ parrentGroupStr ^ "' was not found")
      end

  fun getProjectGroupNameStr ({project, ...}: t) = JSON.stringOf (getNameFromNode (getProjectNodeFromProject project))


  fun getFileNamesInGroup (r as {project, ...}: t) parrentGroupStr =
      let       
        val node = locateParrentGroup parrentGroupStr (getProjectNodeFromProject project)

       fun extractFiles ((node as JSON.Object _), res) = JSON.fold extractFiles res ((getNodesFromValue o getValueFromNode) node)
         | extractFiles ((JSON.String s), res) = StringOrderedSet.insert res s
         | extractFiles (x,res) = die ("Only JSON.Object and JSON.String are allowed in the 'Nodes' list: " ^ JSON.write x)
      in
        extractFiles (node, StringOrderedSet.empty)     
      end          
 
  fun getFileNames (r as {project, ...}: t)  = getFileNamesInGroup r (getProjectGroupNameStr r)                                   

  (* TODO: Right now we validate if the file is already in the project. But we
     don't validate properly if the new file or the compared file from the
     porject is relative and the other is absolute (or visa versa). *)
  fun addFile (r as {project, ...}: t) parrentGroupStr filenameStr = 
      let
        val files = getFileNames r
        val _ = StringOrderedSet.member files filenameStr andalso die ("The file: '" ^ filenameStr ^  "' is already present in this project")

        fun addFileToNodes nodes = (true, JSON.++(JSON.String filenameStr, nodes))
              

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

  (* TODO: also rename depends and expose information *)
  (* TODO: Check whether the new name introduces conclics (The new name is already used by a file or group elsewere in the project) *)
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
              val newNodes = JSON.++(newGroup, nodes)
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

  (* TODO: Also rename depends and expose information *)
  (* TODO: Check whether the new name introduces conclics (The new name is already used by a file or group elsewere in the project) *)
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
                     
        (* Make sure that the depFrom is a valid group or file in the project *)
        val _ = not (StringOrderedSet.member files depFrom orelse StringOrderedSet.member groups depFrom)
                andalso die ("The dependency from ('"^ depFrom ^"') is not a valid file or group in the project")

        (* Make sure that the depTo is a valid group or file in the project *)
        val _ = not (StringOrderedSet.member files depTo orelse StringOrderedSet.member groups depTo)
                andalso die ("The dependency to ('"^ depTo ^"') is not a valid file or group in the project")

        (* Make sure the new dependency is not from the project group as this
           makes no sence or to the project group as this would create cyclic
           dependencies. *)
        val _ = 
            let 
              val projectGroupNameStr = getProjectGroupNameStr r
            in
              (projectGroupNameStr = depFrom andalso die ("It is not allowed to make dependencies from the project group")) orelse
              (projectGroupNameStr = depTo andalso die ("It is not allowed to make dependencies to the project group"))
            end            
                               
        fun insertDepend dependsLst =
            let
              (* If the depTo exists in the list then die, else add it to the list.  *)
              val _ = (JSON.exists (fn x => (JSON.stringOf x) = depTo) dependsLst) andalso die ("The depends list already contains a dependency to '" ^ depTo )
            in
              JSON.++(JSON.String depTo, dependsLst)
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
                                 JSON.++((Generate.dependency depFrom depTo), newDependencies))
                                 
                                 
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


  fun getExposes (r as {project, ...}: t) = 
      let
        (* res is a stringorderedset of group- and filenames *)
        fun extractExposes (JSON.String s, res) = StringOrderedSet.insert res s
          | extractExposes (x, res) = die ("Expected a JSON String as the group- or filename to be exposed, but got: " ^ JSON.write x)

        (* res is dictionary of groupname and stringorderedset of exposed group- and filenames *)
        fun extractExposesFromNode ((node as JSON.Object _), res) = 
            let
              val nameStr = JSON.stringOf (getNameFromNode node)
              val value = getValueFromNode node
              val nodes = getNodesFromValue value
              val exposes = getExposesFromValue value

              (* As it is not allowed to have multiple groups with same name, we
              can blindly insert/update the list of exposed group- and filenames for
              each group we find *)
              val res' = Dictionary.update res (nameStr, JSON.fold extractExposes StringOrderedSet.empty exposes)
            in
              (* get all the exposes from the sub groups contained in this group before returning the resulting dictionary *)
              JSON.fold extractExposesFromNode res' nodes
            end
          | extractExposesFromNode (_, res) = res (* Only groups contain expose information , ignore anything else *)
                                                                            
        val projectNode = getProjectNodeFromProject project
      in
        extractExposesFromNode (projectNode, Dictionary.empty)
      end

  fun addExpose (r as {project, ...}: t) parrentGroupStr exposeStr =
      let

        val exposes = getExposes r
        val parrentGroupExposes = Dictionary.lookup exposes parrentGroupStr
        val _ = isSome parrentGroupExposes andalso
                StringOrderedSet.member (valOf parrentGroupExposes) exposeStr andalso
                die ("The group- or filename '"^ exposeStr ^"' is already exposed in the group '"^ parrentGroupStr ^"'.")

        fun addExposeToNode node =
            let
              val value = getValueFromNode node
              val newExposes = JSON.++(JSON.String exposeStr, getExposesFromValue value)
              val newValue = updateObject value "Exposes" newExposes
              val newNode = updateObject node "Value" newValue
            in
              (true, newNode)
            end
            
        fun locateNodeToAddExpose (node as JSON.Object _) =
            let
              val nameStr = JSON.stringOf (getNameFromNode node)
            in
              if nameStr = parrentGroupStr then (* this is the parrent group, add the expose *) 
                addExposeToNode node
              else (* seach the sub groups for a matching parrent group *)
                let
                  val value = getValueFromNode node
                  val nodes = getNodesFromValue value
                              
                  val (modified, newNodes) = JSON.mapUntil locateNodeToAddExpose nodes
                in
                  if modified then
                    let
                      val newValue = updateObject value "Nodes" newNodes
                      val newNode = updateObject node "Value" newValue
                    in
                      (modified, newNode)
                    end
                  else
                    (false, node)
                end 
            end              
          | locateNodeToAddExpose x = (false, x) (* only groups contain expose information, ignore anything else *)

        val (modified, newProjectNode) = locateNodeToAddExpose (getProjectNodeFromProject project)

        val newProject = if modified then
                           updateObject project "ProjectNode" newProjectNode
                         else
                           die ("The Group '"^ parrentGroupStr ^"' was not found")
      in
        updateProjectInRecord r newProject
      end

  (* TODO: validate that the expose to be removed is in the list of exposed ..  *)
  fun removeExpose (r as {project, ...}: t) parrentGroupStr exposeStr = 
      let
        fun removeExposeFromNode node =
            let
              val value = getValueFromNode node
              val (modified, newExposes) = JSON.filterUntil (fn x => (JSON.stringOf x) = exposeStr) (getExposesFromValue value)
            in
              if modified then
                let
                  val newValue = updateObject value "Exposes" newExposes
                  val newNode = updateObject node "Value" newValue
                in
                  (true, newNode)
                end
              else
                die ("The group- or filename to remove ('" ^ exposeStr ^ "') was not found.")
            end
            
        fun locateNodeToAddExpose (node as JSON.Object _) =
            let
              val nameStr = JSON.stringOf (getNameFromNode node)
            in
              if nameStr = parrentGroupStr then (* this is the parrent group, add the expose *) 
                removeExposeFromNode node
              else (* seach the sub groups for a matching parrent group *)
                let
                  val value = getValueFromNode node
                  val nodes = getNodesFromValue value
                              
                  val (modified, newNodes) = JSON.mapUntil locateNodeToAddExpose nodes
                in
                  if modified then
                    let
                      val newValue = updateObject value "Nodes" newNodes
                      val newNode = updateObject node "Value" newValue
                    in
                      (modified, newNode)
                    end
                  else
                    (false, node)
                end 
            end              
          | locateNodeToAddExpose x = (false, x) (* only groups contain expose information, ignore anything else *)

        val (modified, newProjectNode) = locateNodeToAddExpose (getProjectNodeFromProject project)

        val newProject = if modified then
                           updateObject project "ProjectNode" newProjectNode
                         else
                           die ("The Group '"^ parrentGroupStr ^"' was not found")
      in
        updateProjectInRecord r newProject      
      end


  val setProperty = unimp;

  fun getProperties ({project, ...}: t) = JSON.dictionaryOf (getPropertiesFromProject project)  

  fun listFilesAndGroups (r as {project, ...}: t)  =
      let

        fun exposesToString indent node = 
            let
              val exposes = (getExposesFromValue o getValueFromNode) node
            in
              if List.length (JSON.listOf exposes) > 0 then
                (JSON.fold (fn (a,b) => b ^ indent ^ "|= '" ^ (JSON.stringOf a) ^ "'\n") (indent ^ "|  This group expose the following:\n") exposes)
                ^ indent ^ "| \n"
              else
                ""
            end                  

        (* pretty print group names with indent for each sub group *)
        fun listFilesAndGroups' indent ((node as JSON.Object _), res) = 
            let
              val nameStr = JSON.stringOf (getNameFromNode node)    
              val nodes = (getNodesFromValue o getValueFromNode) node
                          
              val res' = res ^ (indent ^ "|- G: '" ^ nameStr ^ "'\n" ^ (exposesToString ("   " ^ indent) node))
            in
              JSON.fold (listFilesAndGroups' ("   " ^ indent)) res' nodes
            end
          | listFilesAndGroups' indent ((JSON.String s), res) = res ^ (indent ^ "|- F: '" ^ s ^ "'\n")
          | listFilesAndGroups' _ (x,res) = res (* ignore anything else *)

        val projectNode = getProjectNodeFromProject project
        val projectNameStr = JSON.stringOf (getNameFromNode projectNode)

        val projectNodes = (getNodesFromValue o getValueFromNode) projectNode
      in
        "Labels: (G)roup, (F)ile\n" ^
        "-------------------------------\n" ^
        (JSON.fold (listFilesAndGroups' "") ("G: '" ^ projectNameStr ^ "'\n" ^ (exposesToString "" projectNode)) projectNodes) ^
        "\n"
      end

  fun projectToString (r as {project,...}: t) = (JSON.show project) ^ "\n";

  fun toString (r as {project, path,...}: t) =
      "project file: " ^ Path.path path ^ "\n" ^
      "Project: \n" ^ JSON.show project;

        
  val show = unimp;


  structure MLB =
  struct

  fun sortFilesAndGroups (r as {project, ...}: t) filesLst groupsLst dependenciesLst exposesLst =
      let
        fun isGroup nameStr = List.exists (fn x => x = nameStr) groupsLst
                              
        fun expandGroupInDepend (dependStr, res) = 
              (* if this depend is in the list of groups, then it must be a group (obviously) *)
              if isGroup dependStr then
                let
                  (* StringOrderedSet of filenames contained in this group *)
                  val files = getFileNamesInGroup r dependStr
                in
                  (StringOrderedSet.toList files) @ res
                end
              else
                dependStr :: res
                                                              
        (* expand any reference to a group with alle the filenames below that group  *)
        fun expandGroupToFiles (dependency as (depFrom, depends), res) = 
            let
              (* expand this dependency if the dependency is from a group *)
              val newDepFromsLst = expandGroupInDepend(depFrom, [])

              (* expand any groups in the depends list. *)
              val newDependsLst = List.foldl expandGroupInDepend [] (StringOrderedSet.toList depends)
            in
              (Utils.pairOneFromEach newDepFromsLst newDependsLst) @ res
            end

        fun getGroupToGroupDependencies (dependency as (depFrom, depends), res) = 
              if isGroup depFrom then (* only look at dependencies from groups  *)
                let
                  val newDependsLst = List.filter isGroup (StringOrderedSet.toList depends)
                in
                  (Utils.pairOneFromEach [depFrom] newDependsLst) @ res
                end
              else (* remove the dependency *)
                res

        val projectNameStr = getProjectGroupNameStr r

        val expandedFileDependenciesPairLst = List.foldl expandGroupToFiles [] dependenciesLst
                                              
        val groupToGroupDependenciesPairLst = List.foldl getGroupToGroupDependencies [] dependenciesLst

        val _ = (print "The expanded file dependencies are:\n";
                 List.app (fn (a,b) => print (a ^ " => " ^ b ^ "\n")) expandedFileDependenciesPairLst;
                 print "\n\n")

        val _ = (print "The group to group dependencies are:\n";
                 List.app (fn (a,b) => print (a ^ " => " ^ b ^ "\n")) groupToGroupDependenciesPairLst;
                 print "\n\n")


        (* TODO: Make a small improvement. In the resulting topologically sorted
                 list, only include files that are actually used in the project.

                 For example, dont include files that dont have dependencies to
                 them and are not exposed by a group.
         *)


        val sortedFilenames = TopologicalSort.sort filesLst expandedFileDependenciesPairLst
        val sortedGroupnames = TopologicalSort.sort (List.filter (fn x => x <> projectNameStr) groupsLst) groupToGroupDependenciesPairLst 

        val _ = (print "\n\n\n\n\n\n";
                 print "The topologically sorted list of filenames are:\n";
                 List.app (fn x => print (x ^ "\n")) sortedFilenames;
                 print "\n\n";
                 print "The topologically sorted list of groupsnames are:\n";
                 List.app (fn x => print (x ^ "\n")) sortedGroupnames;
                 print "\n\n")
      in
        (* Calls rev, as the sorted file/groups are with elements depending on
           other elements first. Does this this comment even make sence? Anyway
           the two lists are in the wrong order of what the rest of the code
           expects.*)
        (rev sortedFilenames, expandedFileDependenciesPairLst, rev sortedGroupnames, groupToGroupDependenciesPairLst)
      end

  (* TODO: Translate filenames to absolute paths if they are not already. *)
  fun translateFiles (r as {project, ...}: t) sortedFiles fileToFileDepsLst =
      let
        fun translateFiles' (file :: files) num translatedFiles filesToBasisMapping =
            let
              (* if file is a relative path, it returns the absolute path of projectPath^file. *)
              val absFilePath = OS.Path.mkAbsolute {path=file, relativeTo=(OS.Path.dir (getProjectPath r))}
              val basisName = String.translate (fn #"." => "_" | s => Char.toString s ) ((OS.Path.file absFilePath) ^ "_" ^ (Int.toString num))

              (* Update the mapping from original filename to the generated basisname *)
              val filesToBasisMapping' = Dictionary.update filesToBasisMapping (file, basisName)
                              
              (* Collect the dependencies from this file *)
              val filteredDepsLst = List.filter (fn (depFrom, depTo) => depFrom = file) fileToFileDepsLst

              (* Generate a list of files this file depends on *)
              val thisFileDepsOnLst = List.foldl (fn ((depFrom, depTo), b) => depTo :: b) [] filteredDepsLst


              (* Translate the deps to their previously generated basisnames.
                 Since the list of files is topologically sorted the dependency
                 should already have been translated and thus put into the
                 mapping dictionary. So we can blindly do a valOf the result *)
              val thisFileDepsOnBasisLst = List.map (fn depTo => valOf (Dictionary.lookup filesToBasisMapping depTo)) thisFileDepsOnLst

              val fileDescription = if (List.length thisFileDepsOnBasisLst) > 0 then
                                      ("basis " ^ basisName ^ " = bas let "
                                       ^ List.foldl (fn (basisName, b) => b ^ " " ^ basisName) "open" thisFileDepsOnBasisLst
                                       ^ " in "
                                       ^ absFilePath
                                       ^ " end end")                                
                                    else
                                      ("basis " ^ basisName ^ " = bas "
                                       ^ absFilePath
                                       ^ " end")
            in
              translateFiles' files (num+1)  (fileDescription :: translatedFiles) filesToBasisMapping'
            end
            (* the translated files have ben reversed while looping throught
            them, so reverse them agian *)
          | translateFiles' [] num translatedFiles filesToBasisMapping  = (num, rev translatedFiles, filesToBasisMapping)
      in
        translateFiles' sortedFiles 0 [] Dictionary.empty
      end

  fun translateGroups sortedGroups groupToGroupDepsLst exposesLst basisMapping num =
      let
        fun translateGroups' (group :: groups) num translatedGroups basisMapping =
            let
              val basisName = group ^ "_" ^ (Int.toString num)

              val basisMapping' = Dictionary.update basisMapping (group, basisName)

              val filteredDepsLst = List.filter (fn (depFrom, depTo) => depFrom = group) groupToGroupDepsLst

              val thisGroupDepsOnLst = List.foldl (fn ((depFrom, depTo), b) => depTo :: b) [] filteredDepsLst
                                       
              (* A group always have a exposes element, so this list contains an entry
                for each group else the project is not valid and would not have been
                loaded. This list might be empty.*)
              val (_, thisGroupExposes) = valOf (List.find (fn (gp, exposes) => gp = group) exposesLst)

              (* If something is exposed it is also always defined and by the
              topology sort we know that it has already been placed in the *)
              val thisGroupExposesBasisLst = List.map (fn expose => valOf (Dictionary.lookup basisMapping expose)) (StringOrderedSet.toList thisGroupExposes)

              val dependsStr = List.foldl (fn (a,b) => b ^ " " ^ a) "open" thisGroupDepsOnLst
              val exposesStr = List.foldl (fn (a,b) => b ^ " " ^ a) "open" thisGroupExposesBasisLst

              (* TODO: There might be a problem if the group don't expose anything.*)
                               
              val groupDescription = if (List.length thisGroupDepsOnLst) > 0 then
                                       "basis " ^ basisName ^ " = bas "
                                       ^ "let " ^ dependsStr  ^ " in "
                                       ^ exposesStr ^ " end end"
                                     else
                                       "basis " ^ basisName ^ " = bas " ^ exposesStr ^ " end"                                      
            in
              translateGroups' groups (num+1) (groupDescription :: translatedGroups) basisMapping'
            end
          (* the translated groups have ben reversed while looping throught
             them, so reverse them agian *)
          | translateGroups' [] num translatedGroups basisMapping  = (num, rev translatedGroups, basisMapping)
      in
        translateGroups' sortedGroups num [] basisMapping 
      end


                       
  fun translateProjectGroup (r as {project, ...}: t) exposesLst basisMapping  =
      let
        val projectGroupStr = getProjectGroupNameStr r

        (* Again, every group has an , possibly empty, list of exposes *)
        val (_, exposes) = valOf (List.find (fn (group, _) => group = projectGroupStr) exposesLst)

        val exposesBasisLst = List.map (fn expose => valOf (Dictionary.lookup basisMapping expose)) (StringOrderedSet.toList exposes)
                              
      in
        List.foldl (fn (a,b) => b ^ " " ^ a) "open" exposesBasisLst
      end


  fun mlbDescription content  =
          "local\n"
        ^ "  $(SML_LIB)/basis/basis.mlb\n"
        ^ "in\n\n"
        ^ content
        ^ "\nend"
      
  fun description (r as {project, ...}: t) =
      let
        (* list of (depFrom, StringOrderedSet of depends)  *)
        val dependenciesLst = Dictionary.toList (getDependencies r)
        (* list of (group, StringOrderedSet of exposes) *)
        val exposesLst = Dictionary.toList (getExposes r)

        val filesLst = StringOrderedSet.toList (getFileNames r)
        val groupsLst = StringOrderedSet.toList (getGroupNames r)

        val (sortedFiles, fileToFileDepsLst, sortedGroups, groupToGroupDepsLst) = sortFilesAndGroups r filesLst groupsLst dependenciesLst exposesLst

        val (num, translatedFiles, filesToBasisMapping) = translateFiles r sortedFiles fileToFileDepsLst

        val (num, translatedGroups, basisMapping) = translateGroups sortedGroups groupToGroupDepsLst exposesLst filesToBasisMapping num                                                                        

        val content = (List.foldl (fn (a,b) => b ^ "  " ^ a ^ "\n") "" translatedFiles)
                      ^ (List.foldl (fn (a,b) => b ^ "  " ^ a ^ "\n") ""  translatedGroups)
                      ^ "\n  "  
                      ^ (translateProjectGroup r exposesLst basisMapping)                                                                                        
      in
        mlbDescription content
      end

  end

end

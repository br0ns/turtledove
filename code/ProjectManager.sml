structure ProjectManager :> ProjectManager =
struct

type t = {path: Path.t, project : JSON.t}

fun unimp _ = raise Fail "ProjectManager: Unimplemented"

fun die "" = unimp ""
  | die s = raise Fail ("ProjectManager: " ^ s)


structure Generate =
struct 

  val emptyValueStr = "{ \"Exposes\": [], \"Nodes\": [] }"
  val emptyValue = JSON.read emptyValueStr
  
  fun emptyGroupStr name = "{\"Name\": \"" ^ name ^ "\", \"Value\": " ^ emptyValueStr  ^ "}"
  val emptyGroup = JSON.read o emptyGroupStr

  fun emptyProjectStr name = "{ \"Properties\" : { }, \"ProjectNode\" : " ^ (emptyGroupStr name)   ^ "}"
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
       TextIO.output (fs, (JSON.write project));
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
      TextIO.output (fs, (JSON.write project)) before TextIO.closeOut fs
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


(* 
 * Rekursive function that loops the parseNode function through each element in the
 * JSON.Array. The parseNode function just need to return the possibly updated res 
 * variable back when done parsing that single node.
 *
 * The parseJSONArray function is responsible for handling if the node is a unexpected JSON type
 *)

(* Replace this with the new JSON.map *)
  fun parseJSONArray (JSON.Array lst) parseFun res =
      let
        fun parseJSONArray' (n :: ns) res = parseJSONArray' ns (parseFun n res)
          | parseJSONArray' [] res = res
      in
        parseJSONArray' lst res
      end
    | parseJSONArray x _ _= die ("Expected a JSON Array, but got:" ^ JSON.write x)          


       
  fun getFileNames ({project, ...}: t)  = 
      let
        fun parseFiles (JSON.String s) res = StringOrderedSet.insert res s
          | parseFiles x _ = die ("The list of filenames may only contain JSON String, but contained this element: " ^ JSON.write x)
                             
        fun parseNode (node as JSON.Object _) res = parseJSONArray ((getNodesFromValue o getValueFromNode) node) parseNode res
          | parseNode (JSON.String s) res = StringOrderedSet.insert res s
          | parseNode (files as JSON.Array _) res = parseJSONArray files parseFiles res
          | parseNode x _ = die ("This is not a valid Node: " ^ JSON.write x)         
      in
        parseJSONArray ((getNodesFromValue o getValueFromNode o getProjectNodeFromProject) project ) parseNode StringOrderedSet.empty
      end;
      
  val addFile = unimp;

  val removeFile = unimp;

  val moveFile = unimp;

  val renameFile = unimp;

  fun getProjectGroupName ({project, ...}: t) = JSON.stringOf (getNameFromNode (getProjectNodeFromProject project))

(* There might be a problem here. The structure of which groups belong to who is lost *)
  fun getGroupNames ({project, ...}: t) = 
      let
        fun parseNode (node as JSON.Object _) res =
            let          
              (* The returned field is a JSON.String so writ it to a ordinary string *)              
              val name = JSON.stringOf (getNameFromNode node)
            in
              parseJSONArray ((getNodesFromValue o getValueFromNode) node) parseNode (StringOrderedSet.insert res name)
            end
          (* As group names are only located in the JSON Objects we skip anything else*)
          | parseNode _  res = res
      in
        parseNode (getProjectNodeFromProject project) StringOrderedSet.empty
      end;



  fun addGroup ({project, ...}: t) inGroup newGroup = 
      let
        fun addGroupToNode node =
            let
              val dict = JSON.objectOf node
              val name = getNameFromNode node
              val value = getValueFromNode node
              val nodes = getNodesFromValue value
            in
              if name = inGroup then
                let 
                  val newGroup = Generate.emptyGroup newGroup
                  val newNodes = JSON.cons(newGroup, nodes)
                  val newValue = updateDict value "Nodes" newNodes
                  val newNode = updateDict node "Value" newValue
                in (* break the mapping *)
                  (true, newNode )
                end
              else (* Look in this groups nodes to se if it is a subgroup and break if so. *)
                let
                  val (added, newNodes) = JSON.mapUntil addGroupToNode nodes
                  val newValue = updateDict value "Nodes" newNodes
                  val newNode = updateDict node "Value" newValue
                in
                  (added, newNode)
                end
            end

        val projectNode = getProjectNodeFromProject project
        val (_, newProjectNode) = addGroupToNode projectNode
        val newProject = JSON.Object (updateDict (JSON.objectOf project) "ProjectNode" newDependencies)
      in
        updateProjectInRecord r newProject
      end


  val removeGroup = unimp;


  val moveGroup = unimp;

  val renameGroup = unimp;

  fun getDependencies ({project, ...}: t) =
      let
        fun parseDependency (node as JSON.Object dict) res =
            let
              fun parseDepend (JSON.String s) res = StringOrderedSet.insert res s
                | parseDepend x _ = die ("Expected a JSON String as 'Depends' constraint, but got: " ^ JSON.write x)

              val nameStr = JSON.stringOf (getNameFromNode node)
              val depends = getDependsFromNode node
              val dep = Dictionary.lookup res nameStr
            in
              if isSome dep then
                Dictionary.update res (nameStr, parseJSONArray depends parseDepend (valOf dep))
              else
                Dictionary.update res (nameStr, parseJSONArray depends parseDepend StringOrderedSet.empty)
            end
          | parseDependency x _ = die ("Expected a JSON Object as dependency to lookup in, but got: " ^ JSON.write x)
      in
        parseJSONArray (getDependenciesFromProject project) parseDependency Dictionary.empty
      end;

  fun addDependency (r as {project, ...}: t) depFrom depTo =
      let  
        fun parseDependency (node as JSON.Object dict) (true, res) = (true, node :: res)
          | parseDependency (node as JSON.Object dict) (match, res) =
            let
              val name = JSON.stringOf (getNameFromNode node)
            in
              if (name = depFrom) then (* match, update the depends with the depTo *)
                let
                  val depends = getDependsFromNode node
                  val newDepends = JSON.Array( JSON.String depTo :: (JSON.arrayOf depends))
                  val newDependency = JSON.Object (updateDict dict "Depends"  newDepends)
                in
                  (true, (JSON.Object (updateDict dict "Depends" newDepends)) :: res)
                end                 
              else (* no match, keep going *)
                 (match, node :: res)
            end
          | parseDependency x _ = die ("Expected a JSON Object as dependency to lookup in, but got: " ^ JSON.write x)
  
        val dependencies = getDependenciesFromProject project
        val (match, newDependenciesLst) = parseJSONArray dependencies parseDependency (false, [])
        val newDependencies = JSON.Array
                                  (if match then (* match, we have update the list *)
                                      (rev newDependenciesLst )
                                   else (* No match, insert the dependency *)
                                      (JSON.read ("{\"Name\" : \"" ^ depFrom ^ "\", \"Depends\" : [ \"" ^ depTo ^ "\" ]}")) :: (rev newDependenciesLst))
                                                            
        val newProject = JSON.Object (updateDict (JSON.objectOf project) "Dependencies" newDependencies)
      in
        updateProjectInRecord r newProject
      end

  fun removeDependency (r as {project, ...}: t) depFrom depTo =
      let
        fun parseDependency (node as JSON.Object dict) res =
            let
              val name = JSON.stringOf (getNameFromNode node)
            in
              if (name = depFrom) then (* match, remove the objec *)
                let
                  val depends = getDependsFromNode node
                  val newDepends = JSON.Array (List.filter (fn s => (JSON.stringOf s) <> depTo) (JSON.arrayOf depends))
                  val newDependency = JSON.Object (updateDict dict "Depends"  newDepends)                
                in
                  (JSON.Object (updateDict dict "Depends" newDepends)) :: res
                end
              else (* no match, keep going *)
                node :: res
            end
          | parseDependency x _ = die ("Expected a JSON Object as dependency to lookup in, but got: " ^ JSON.write x)
        
        val dependencies = getDependenciesFromProject project
        val newDependenciesLst = parseJSONArray dependencies parseDependency []
        val newDependencies = JSON.Array (rev newDependenciesLst)

        val newProject = JSON.Object (updateDict (JSON.objectOf project) "Dependencies" newDependencies)
      in
        updateProjectInRecord r newProject
      end

  val setProperty = unimp;

  fun getProperties ({project, ...}: t) = JSON.objectOf (getPropertiesFromProject project)  


  fun toString {path=path, project = project} =
      "project file: " ^ Path.path path ^ "\n" ^
      "Project:\n " ^ JSON.write project;

        
  val show = unimp;


end;

structure ProjectManager :> ProjectManager =
struct

type t = {path: Path.t, project : JSON.t}

fun unimp _ = raise Fail "ProjectManager: Unimplemented"

fun die "" = unimp ""
  | die s = raise Fail ("ProjectManager: " ^ s)


fun init name projectPath = 
    let
      val path = Path.new projectPath
      val project = JSON.read (
"{" ^
" \"Properties\" : { }," ^
" \"ProjectNode\" : " ^
"  {" ^
"   \"Name\"  : \"" ^ name  ^ "\"," ^
"   \"Value\" : " ^
"    {" ^
"     \"Exposes\" : [ ]," ^
"     \"Depends\" : [ ]," ^
"     \"Nodes\" : [ ]" ^
"    }" ^
"  }" ^
"}")
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
    
fun saveProject {path = path, project = project} = 
    let
      val fs = File.openOut path
    in      
      TextIO.output (fs, (JSON.write project)) before TextIO.closeOut fs
    end;

fun getFieldFromProjectObject (JSON.Object dict) field = valOf (Dictionary.lookup dict field)
  | getFieldFromProjectObject x _ = die ("Expected a JSON Object as toplevel Project description, but got: " ^ JSON.write x)

fun getFieldFromNodeObject (JSON.Object dict) field = valOf (Dictionary.lookup dict field)
  | getFieldFromNodeObject x _ = die ("Expected a JSON Object as 'Node' field, but got: " ^ JSON.write x)

and getFieldFromValueObject (JSON.Object dict) field = valOf (Dictionary.lookup dict field)
  | getFieldFromValueObject x _  = die ("Expected a JSON Object as 'Value' field, but got: " ^ JSON.write x)


    (* This assumes that the project is well formattet *)
  fun getFileNames {path = path, project = project}  = 
      let
        fun parseNodes (JSON.Array lst) res = 
            let
              fun parseNodes' ((node as JSON.Object _) :: xs) res = parseNodes' xs (parseNode node res)
                | parseNodes' ((JSON.String s) :: xs) res = parseNodes' xs (StringOrderedSet.insert res s)
                | parseNodes' ((JSON.Array lst) :: xs) res = 
                  let
                    fun parseFiles ((JSON.String s) :: xs) res = parseFiles xs (StringOrderedSet.insert res s)
                      | parseFiles [] res = res
                      | parseFiles (x :: xs) _ = die ("The list of filenames may only contain JSON String, but contained this element: " ^ JSON.write x)
                  in
                    parseNodes' xs (parseFiles lst res)
                  end
                | parseNodes' [] res = res
                | parseNodes' (x::xs) _ = die ("This is not a valid Node: " ^ JSON.write x)
            in
              parseNodes' lst res
            end
          | parseNodes x _= die ("Expected a JSON Array as 'Nodes' field, but got:" ^ JSON.write x)

        fun getValueFromNode node = getFieldFromNodeObject node "Value"                
        val projectNode = getFieldFromProjectObject project "ProjectNode"

        fun                           
      in
        parseNode () StringOrderedSet.empty
      end;

  val addFile = unimp;

  val removeFile = unimp;

  val moveFile = unimp;



  val getGroupNames = unimp;


  val addGroup = unimp;


  val removeGroup = unimp;


  val moveGroup = unimp;


  val getDependencys = unimp;

  val addDependency = unimp;

  val getProperties = unimp;
  val setProperty = unimp;

  fun toString {path=path, project = project} =
      "project file: " ^ Path.path path ^ "\n" ^
      "Project:\n " ^ JSON.write project;

        
  val show = unimp;


end;

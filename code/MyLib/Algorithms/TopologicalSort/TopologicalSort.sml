structure TopologicalSort :> TopologicalSort =
struct

fun die s = raise Fail ("TopologicalSort: " ^ s);

(* 

The topological sorting is based on "depth-first search", and is the one described in Cormen et al,

For this algorithm, edges point in the opposite direction. There is an edge from
x to y if job x depends on job y (in other words, if job y must be completed
before job x can be started).

L : Empty list that will contain the sorted nodes
S : Set of all nodes

function visit(node n)
    if n has not been visited yet then
        mark n as visited
        for each node m with an edge from n to m do
            visit(m)
        add n to L

for each node n in S do

The algorithm can be refined to detect cycles by watching for nodes which are
visited more than once during any nested sequence of recursive calls to visit()
(e.g., by passing a list down as an extra argument to visit(), indicating which
nodes have already been visited in the current call stack)

*)


fun reverseEdges edges =
    let
      fun reverseEdges' ((from, to) :: xs) res = reverseEdges' xs ((to, from) :: res)
        | reverseEdges' [] res =  res
    in
      reverseEdges' edges []
    end

fun getEdgesFromNode node edges =
    let
      fun getEdges' ((edge as (from, _)) :: xs) res =
          if node = from then
            getEdges' xs (edge :: res)
          else
            getEdges' xs res
        | getEdges' [] res = res
    in
      getEdges' edges []
    end

(* Locate the node in the list and returns the list in some order with the
   located node at the front. Returns NONE if the node was not found *)
fun locateNode node nodes =
    let
      fun locateNode' (x :: xs) res =
          if node = x then
            SOME (x :: (xs @ res))
          else
            locateNode' xs (x :: res)
        | locateNode' [] res = NONE
    in
      locateNode' nodes []
    end
    

(* 
 * In this implementation of visit, we mark nodes a visited by removing them
 * from the list of total nodes, as the recursive calls stops if it reaches a
 * node that has already been visited.
 *
 * Also a list of visited nodes are passed along recursive calls, to detect
 * cycles.
 *)

fun sort nodes edges = 
    let
      
      val revEdges = (*reverseEdges*) edges 
                        
      fun dieIfCycle visitedNodes edgeTo =
          (* report if we have an edge to a node that has already been visited. *)
          List.exists (fn x => x = edgeTo) visitedNodes 
          andalso die ("A cycle was found:" ^ (List.foldl (fn (a,"") => a | (a,b) => b ^ " -> " ^ a) ""  (edgeTo :: visitedNodes))) 
                  
      fun visitNodesInEdges [] visitedNodes ((edgeFrom, edgeTo) :: es) sortedLst = 
          (dieIfCycle visitedNodes edgeTo; 
           print "No more nodes to visit.\n";
           ([], sortedLst))
        | visitNodesInEdges nodes visitedNodes [] sortedLst = 
          (print "No more edges from this node\n"; 
           (nodes, sortedLst))
        | visitNodesInEdges nodes visitedNodes (edgesFromNode as ((edgeFrom, edgeTo) :: es)) sortedLst = 
          let
            val _ = (print "Visited nodes: ";
                     List.app (fn x => print (x ^ " ")) visitedNodes;
                     print "\n")          

            val _ = dieIfCycle visitedNodes edgeTo
                    
            val nodes' = locateNode edgeTo nodes

          in
            if isSome nodes' then (* the node was found and thus has not been visited before *)
              let

                val _ = (print "Nodes': ";
                         List.app (fn x => print (x ^ " ")) (valOf nodes');
                         print "\n";
                         print ("Following dependency to " ^ edgeTo ^ "\n"))

                val (newNodes, sortedLst') = visit' (valOf nodes') visitedNodes sortedLst

                val _ = (print "Finished following dependency. Remaining dependencys are:\n";
                         List.app (fn (a,b) => print (a ^ " -> " ^ b ^ "\n")) es)                        

              in
                visitNodesInEdges newNodes visitedNodes es sortedLst'
              end
            else (* the node was not found, and thus it has been visited before so stop following this path. *)
              (print "Could not find the node refered to in the depTo. Must have been visited already\n";
               visitNodesInEdges nodes visitedNodes es sortedLst)
          end                    
                                                              
      and visit' (nodes as (n :: ns)) visitedNodes sortedLst =
          let
            val edgesFromNode = getEdgesFromNode n revEdges

            val _ = (print ("Node: " ^ n ^ "\n");
                     print "Edges from this node: \n";
                     List.app (fn (a,b) => print (a ^ " -> " ^ b ^ "\n")) edgesFromNode)

            val (nodes', sortedLst') = visitNodesInEdges ns (n :: visitedNodes) edgesFromNode sortedLst
          in          
            (nodes', (n :: sortedLst'))
          end
        | visit' [] _ sortedLst = ([], sortedLst)

      and visit (nodes as (n :: ns)) visitedNodes sortedLst =
          let
            val (nodes', sortedLst') = visit' nodes visitedNodes sortedLst
          in
            visit nodes' visitedNodes sortedLst'
          end
        | visit [] _ sortedLst = ([], sortedLst)

      val (_, sortedLst) = visit nodes [] []
                                 
    in
      sortedLst
    end
end

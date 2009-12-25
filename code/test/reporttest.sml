local
    open Report
    infix ++ @@
    val s1 = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec venenatis urna placerat purus elementum rutrum. Nunc arcu mauris, malesuada in convallis quis, elementum non dolor. In dignissim massa sit amet velit aliquet eget adipiscing metus fringilla. Aenean justo velit, tincidunt id rutrum quis, scelerisque et orci. Phasellus adipiscing ultrices commodo. Phasellus vulputate consequat nibh dignissim adipiscing. Vivamus est augue, mattis a scelerisque vitae, viverra et lacus. Donec et magna turpis. Cras commodo diam et arcu gravida suscipit. Vivamus porta elit nec purus vehicula auctor. Nunc enim mi, pellentesque et vestibulum quis, placerat id sem. Curabitur feugiat congue elit. Donec et augue laoreet lectus interdum tristique. Cras at egestas purus. Duis imperdiet convallis fringilla. Nullam sit amet elit non risus elementum mattis. Duis lectus lectus, tincidunt et imperdiet in, iaculis a neque. Phasellus laoreet dignissim est, ac porta diam mollis id. Nullam a nunc eget augue sodales sollicitudin."
    val s2 = "Nam velit dui, hendrerit ac dictum quis, rhoncus sed magna. Fusce urna augue, consequat quis mattis sit amet, congue a lorem. Vestibulum cursus convallis diam varius cursus. Ut eu sapien eu ipsum condimentum porta eu eu ipsum. Aenean tempor, turpis vel condimentum fringilla, orci urna molestie dui, quis eleifend dui orci vitae risus. Nullam at eros tortor. Mauris ante elit, ornare eget varius nec, viverra ut tortor. Nullam feugiat, dolor eu viverra fringilla, diam nulla molestie nisi, elementum egestas orci ante in sem. Fusce malesuada, eros nec suscipit gravida, dolor nisl tincidunt risus, vitae aliquet quam mauris non eros. Integer egestas metus sed lorem tempor sit amet cursus sem imperdiet. Quisque nec neque tortor. Maecenas interdum dui eu tellus aliquam ornare. Aliquam ut volutpat lacus. Donec elementum auctor volutpat. Duis quis diam augue, elementum scelerisque nisi. Quisque semper, mi semper varius fermentum, orci turpis faucibus velit, id interdum nisi odio id nulla. Etiam luctus rutrum lobortis. Proin tincidunt mollis ante, eu aliquam quam rhoncus quis. Nam consequat aliquet velit, semper lacinia tortor facilisis et."
in
(* val _ = print (text "Hej verden" ++ indent (text "Haj sael") ++ text "Roger and over") *)
(* val _ = print nl *)
val _ = print (enumerate' EnumStyle.Roman
                          [text "lorem ipsum",
                           text "dolor sit amet" ++
                           enumerate [
                           text "punkt 1", 
                           text "punkt 2"
                           ],
                           text "Number 1",
                           text "Number 2",
                           text "Number 3",
                           text "Number 4"])
val _ = print nl

val _ = print (row [paragraph s1, SpaceH 2, paragraph s2])
val _ = print (enumeratenl' EnumStyle.Roman
               [text s1,
                text s2,
                text s1,
                text s2])
val _ = print (itemize nil)
(* val r = text "hej" *)
(* val r' = text "Hello" ++ indent (text "world!") *)
(* val _ = print r *)
(* val _ = print r' *)
(* val _ = print (r @@ text " " @@ r') *)
end

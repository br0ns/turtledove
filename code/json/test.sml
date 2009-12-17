val cnumberlist = JSON.Convert.array JSON.Convert.number
val json = JSON.to cnumberlist (map real [1,2,3])
val _ = print json
val json = JSON.read "{\"bleh\" : {     }}"
val json = JSON.write json
val _ = print json

val draw
  :  ?prms:Gp.prms
  ->  ?display_id:Jupyter_notebook.display_id
  -> ?fmt:[ `png | `svg ]
  -> ?size:int * int
  -> ((module Gp.Plot) -> unit)
  -> unit

val draw
  :  ?display_id:Jupyter_notebook.display_id
  -> ?gnuplot:string
  -> ?fmt:[ `png | `svg ]
  -> ?size:int * int
  -> ?init:string
  -> ((module Gp.Figure) -> unit)
  -> unit

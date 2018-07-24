val draw : ?gnuplot:string -> ?fmt:[ `png | `svg ] -> ?size:int*int -> ?init:string -> ((module Gp.Figure) -> unit) -> unit

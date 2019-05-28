# Gnuplot in Jupyter OCaml notebooks

Please see the [API documentation](http://hennequin-lab.github.io/juplot).

```ocaml
#require "owl, juplot"
open Owl
open Gp

(* make a movie of a drifting sine wave *) 
let _ = 
    let display_id = Jupyter_notebook.display "text/html" "" in
    let plot phase (module F: Figure) = F.ex Printf.(sprintf "plot sin(x+%f)" phase) in
    Mat.linspace 0. Const.pi2 100
    |> Mat.iter (fun phase -> 
        Juplot.draw ~fmt:`svg ~size:(300,200) ~display_id (plot phase);
        Unix.sleepf 0.01) 
```


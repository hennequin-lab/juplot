# Gnuplot in Jupyter OCaml notebooks

Please see the [API documentation](http://hennequin-lab.github.io/juplot).

```ocaml
#require "owl, juplot"
open Owl
open Gp

(* simple example *)
let _ =
  let figure (module F: Figure) =
    F.barebone ();
    let x = Mat.gaussian 20 4 in
    F.plots [ item (A x) ~using:"1:2" ~style:"p pt 7 lc 8";
              item (A x) ~using:"3:4" ~style:"p pt 7 lc 7" ]
  in
  Juplot.draw figure (* PNG format by default *)

(* make a movie of a drifting sine wave *) 
let _ = 
    let display_id = Jupyter_notebook.display "text/html" "" in
    let plot phase (module F: Figure) = F.ex Printf.(sprintf "plot sin(x+%f)" phase) in
    Mat.linspace 0. Const.pi2 100
    |> Mat.iter (fun phase -> 
        Juplot.draw ~fmt:`svg ~size:(300,200) ~display_id (plot phase);
        Unix.sleepf 0.01) 
```


open Printf

let draw ?gnuplot ?size ?init (f : (module Gp.Figure) -> unit) =
  let module O = struct
    include Gp.SVG
    let term =
      let term = match size with Some s -> { term with size = Some s } | None -> term in
      { term with other = Some "mouse standalone" }
    let post_action = 
      Some (fun root ->
          let ic = open_in_bin (root ^ ".svg") in
          let n = in_channel_length ic in
          let data = really_input_string ic n in
          close_in ic ;
          ignore (Jupyter_notebook.display ~base64:false "image/svg+xml" data);
          ())
  end in
  let fig = Gp.figure ?gnuplot ?init ~to_file:"/tmp/juplot_tmp" (module O) in
  let module F = (val fig: Gp.Figure) in
  f (module F);
  F.draw ()


open Printf
open Owl


let figure ?gnuplot ?size ?init () =
  let module O = struct
    include Gp.SVG
    let post_action = 
      Some (fun root ->
          let ic = open_in_bin (root ^ ".svg") in
          let n = in_channel_length ic in
          let data = really_input_string ic n in
          close_in ic ;
          ignore (Jupyter_notebook.display ~base64:false "image/svg+xml" data);
          ())
    let term_opts = match size with
      | Some (w, h) -> term_opts @ [ sprintf "size %i,%i" w h ]
      | None -> term_opts
  end in
  Gp.figure ?gnuplot ?init ~to_file:"/tmp/juplot_tmp" (module O)



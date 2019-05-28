let draw ?display_id ?gnuplot ?(fmt = `png) ?size ?init (f : (module Gp.Figure) -> unit) =
  let (module Out), mime, base64, opts =
    match fmt with
    | `png -> (module Gp.PNG : Gp.Output), "image/png", true, "enhanced transparent crop"
    | `svg -> (module Gp.SVG : Gp.Output), "image/svg+xml", false, "standalone"
  in
  let module O = struct
    include Out

    let term =
      let term =
        match size with
        | Some s -> { term with size = Some s }
        | None -> term
      in
      { term with other = Some opts }


    let post_action =
      Some
        (fun root ->
          let ic = open_in_bin (root ^ file_ext) in
          let n = in_channel_length ic in
          let data = really_input_string ic n in
          close_in ic;
          ignore (Jupyter_notebook.display ?display_id ~base64 mime data);
          ())
  end
  in
  let fig = Gp.figure ?gnuplot ?init ~to_file:"/dev/shm/juplot" (module O) in
  let module F = (val fig : Gp.Figure) in
  f (module F);
  F.draw ()

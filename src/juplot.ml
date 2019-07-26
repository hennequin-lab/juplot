let tmp_root = if Sys.is_directory "/dev/shm" then "/dev/shm" else "/tmp"

let draw ?prms ?display_id ?(fmt = `svg) ?size fig =
  let file = Printf.sprintf "%s/juplot" tmp_root in
  let output, mime, base64, ext =
    match fmt with
    | `png -> Gp.png ?size file, "image/png", true, ".png"
    | `svg ->
      Gp.svg ?size ~other_term_opts:"standalone" file, "image/svg+xml", false, ".svg"
  in
  Gp.draw ?prms ~output fig;
  let ic = open_in_bin (file ^ ext) in
  let n = in_channel_length ic in
  let data = really_input_string ic n in
  close_in ic;
  ignore (Jupyter_notebook.display ?display_id ~base64 mime data)

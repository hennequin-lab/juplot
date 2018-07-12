open Printf
open Owl

module Defaults = struct
  let gnuplot = "gnuplot" (* executable name *)
  let init = "set key noautotitle; \
              set border 3; \
              set tics out nomirror"
  let file_root = "/tmp/juplot_tmp"
  let width = 300
  let height = 200
end


module type Handle = sig
  val h: out_channel
  val mime: string
  val file: string
end


module SVG (P: module type of Defaults) : Handle = struct

  let mime = "image/svg+xml"
  let file = P.file_root ^ ".svg"

  (* create a handle *)
  let h =
    let h = Unix.open_process_out P.gnuplot in
    output_string h "set term svg font \"Helvetica, 10\" square\n";
    output_string h (sprintf "set term svg size %i,%i\n" P.width P.height);
    output_string h (sprintf "set output '%s'\n" file);
    output_string h (P.init ^ "\n");
    h

  (* hack to make sure that gnuplot terminates if the handle is lost *)
  let a = ref 0.
  let _ = Gc.finalise (fun _ -> 
      try ignore (Unix.close_process_out h)
      with _ -> ()) a

end


module PNG (P: module type of Defaults) : Handle = struct

  let mime = "image/png"
  let file = P.file_root ^ ".png"

  (* create a handle *)
  let h =
    let h = Unix.open_process_out P.gnuplot in
    output_string h "set term pngcairo font \"Helvetica, 10\"\n";
    output_string h (sprintf "set term pngcairo size %i,%i\n" P.width P.height);
    output_string h (sprintf "set output '%s'\n" file);
    output_string h (P.init ^ "\n");
    h

  (* hack to make sure that gnuplot terminates if the handle is lost *)
  let a = ref 0.
  let _ = Gc.finalise (fun _ -> 
      try ignore (Unix.close_process_out h)
      with _ -> ()) a

end


module Figure (H: Handle) = struct

  let ex cmd = output_string H.h (cmd^"\n")
  let flush () = flush H.h
  let close () = ignore (Unix.close_process_out H.h)
  let output () = 
    ex "unset output";
    flush ();
    close ();
    let ic = open_in_bin H.file in
    let n = in_channel_length ic in
    let data = really_input_string ic n in
    close_in ic ;
    ignore (Jupyter_notebook.display ~base64:true H.mime data);
    ()

  let end_signal () = fprintf H.h "e\n%!"

  let __send_columns m =
    let cols = Array.length m in
    let rows = Array.fold_left max (-1) (Array.map Array.length m) in
    for i=0 to rows-1 do
      for j=0 to cols-1 do
        let mj = m.(j) in
        if i<Array.length mj
        then fprintf H.h "%f " mj.(i)
        else fprintf H.h "- ";
      done; fprintf H.h "\n%!";
    done;
    end_signal ()

  let send_columns m =
    __send_columns (Array.map (fun x -> x |> Mat.to_array) m)


  (* for some reason, gnuplot wants a double "e" at the end
     of the stream for matrix data given to [splot()] ... *)

  let send_matrix m =
    send_columns (Mat.to_cols m);
    end_signal ()

  let plot data =
    let cmds = Array.map (fun (_,opts) -> sprintf "'-' %s" opts) data |> Array.to_list in
    ex ("plot " ^ String.concat ", " cmds);
    Array.iter (fun (d,_) -> send_columns (Array.of_list d)) data

  let splot data =
    let mat, opts = data in
    ex "set pm3d map";
    let n, m = Mat.shape mat in
    List.iter ex [
      sprintf "set xrange [%f:%f]" (-0.5) (float n -. 0.5);
      sprintf "set yrange [%f:%f] reverse" (-0.5) (float m -. 0.5);
      sprintf "splot '-' %s" opts
    ];
    send_matrix mat

  let image mat =
    let n, m = Mat.shape mat in
    List.iter ex [
      sprintf "set xrange [%f:%f]" (-0.5) (float m -. 0.5);
      sprintf "set yrange [%f:%f] reverse" (-0.5) (float n -. 0.5);
      "plot '-' mat w image"];
    send_matrix mat


  (* set properties *)

  type _ property =
    | Title : string property
    | XLabel : string property
    | YLabel : string property
    | ZLabel : string property
    | XRange : (float * float) property
    | YRange : (float * float) property
    | ZRange : (float * float) property

  let set (type a) ?o (prop: a property) (x: a) = 
    let cmd = match prop with
      | Title -> sprintf "set title '%s'" x

      | XLabel -> sprintf "set xlabel '%s'" x
      | YLabel -> sprintf "set ylabel '%s'" x
      | ZLabel -> sprintf "set zlabel '%s'" x

      | XRange -> sprintf "set xrange [%f:%f]" (fst x) (snd x)
      | YRange -> sprintf "set yrange [%f:%f]" (fst x) (snd x)
      | ZRange -> sprintf "set zrange [%f:%f]" (fst x) (snd x)
    in
    let cmd = match o with Some o -> sprintf "%s %s" cmd o | None -> cmd in
    ex cmd

  let margins = List.iter (function
      | `T x -> ex (sprintf "set tmargin at screen %f" x)
      | `B x -> ex (sprintf "set bmargin at screen %f" x)
      | `L x -> ex (sprintf "set lmargin at screen %f" x)
      | `R x -> ex (sprintf "set rmargin at screen %f" x))

  let multiplot rows cols vspace hspace plot_fun =
    ex (sprintf "set multiplot layout %i,%i" rows cols);
    let hrem = 2. *. hspace *. float cols
    and vrem = 2. *. vspace *. float rows in
    let width = (1. -. hrem) /. float cols
    and height = (1. -. vrem) /. float rows in
    for k=0 to rows*cols - 1 do 
      let row = k/cols and col = k mod cols in
      let t = 1. -. hspace -. 2. *. hspace *. float row 
              -. height *. float row in
      let b = t -. height in
      let l = 1. -. (1. -. vspace -. 2. *. vspace *. float col 
                     -. width *. float col) in
      let r = l +. width in
      margins [`T t; `B b; `L l; `R r];
      plot_fun k row col;
    done;
    ex "unset multiplot"

end



module Quick ( ) = Figure (PNG (Defaults))




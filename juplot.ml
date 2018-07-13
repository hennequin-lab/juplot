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
  val base64: bool
  val mime: string
  val file: string
end


module SVG (P: module type of Defaults) : Handle = struct

  let base64 = false
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

  let base64 = true
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


(* set properties *)

type axis = [ `x | `x2 | `y | `y2 | `z | `cb ]

let string_of_axis = function 
  | `x -> "x" 
  | `x2 -> "x2" 
  | `y -> "y" 
  | `y2 -> "y2" 
  | `z -> "z" 
  | `cb -> "cb"

type _ property =
  | Title       : string property
  | Label       : (axis * string) property
  | Range       : (axis * (float * float)) property
  | Tics        : (axis * ([ `list of (float * string) list 
                           | `def of (float * float * float)])) property
  | Key         : string property
  | Palette     : string property
  | Format      : (axis * string) property
  | Autoscale   : axis property
  | Logscale    : axis property
  | Text        : (int * string) property
  | Border      : [ `L | `R | `T | `B ] list property
  | Colorbox    : string property
  | Prop        : string property

type _ unset_property =
  | Title       : unit unset_property
  | Label       : axis unset_property
  | Tics        : axis unset_property
  | Key         : unit unset_property
  | Autoscale   : axis unset_property
  | Logscale    : axis unset_property
  | Text        : int unset_property
  | Border      : unit unset_property
  | Colorbox    : unit unset_property
  | Prop        : string unset_property


module Figure (H: Handle) = struct

  let ex cmd = output_string H.h (cmd^"\n")
  let flush () = flush H.h
  let close () = ignore (Unix.close_process_out H.h)
  let display () = 
    ex "unset output";
    flush ();
    close ();
    let ic = open_in_bin H.file in
    let n = in_channel_length ic in
    let data = really_input_string ic n in
    close_in ic ;
    ignore (Jupyter_notebook.display ~base64:H.base64 H.mime data);
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



  let load s = ex (sprintf "load '%s'" s)


  let set (type a) ?o (prop: a property) (x: a) = 
    let cmd = match prop with
      | Title -> sprintf "set title '%s'" x
      | Label -> 
        let ax, lbl = x in
        sprintf "set %slabel '%s'" (string_of_axis ax) lbl
      | Range ->
        let ax, (a, b) = x in
        sprintf "set %srange [%f:%f]" (string_of_axis ax) a b
      | Tics -> 
        let ax, ti = x in
        sprintf "set %stics %s" (string_of_axis ax)
          (match ti with
           | `list s -> 
             let z = String.concat ", " (List.map (fun (x,la) -> sprintf "'%s' %f" la x) s) in
             sprintf "( %s )" z
           | `def (a0,step,a1) -> sprintf "%f, %f, %f" a0 step a1)
      | Key -> sprintf "set key %s" x
      | Palette -> sprintf "set palette %s" x
      | Format -> 
        let ax, fmt = x in
        sprintf "set format %s %s" (string_of_axis ax) fmt
      | Autoscale -> sprintf "set autoscale %s" (string_of_axis x) 
      | Logscale -> sprintf "set logscale %s" (string_of_axis x) 
      | Text ->
        let id, lbl = x in
        sprintf "set label %i %s" id lbl
      | Border ->
        let total = List.fold_left (fun accu side ->
            accu + match side with `B -> 1 | `L -> 2 | `T -> 4 | `R -> 8) 0 x in
        sprintf "set border %i" total
      | Colorbox -> sprintf "set colorbox %s" x
      | Prop -> sprintf "set %s" x

    in
    let cmd = match o with Some o -> sprintf "%s %s" cmd o | None -> cmd in
    ex cmd

  let unset (type a) (prop: a unset_property) (x: a) = 
    let cmd = match prop with
      | Title -> "unset title"
      | Label -> sprintf "unset %slabel" (string_of_axis x)
      | Tics -> sprintf "unset %stics" (string_of_axis x)
      | Key -> "unset key"
      | Autoscale -> sprintf "unset autoscale %s" (string_of_axis x)
      | Logscale -> sprintf "unset logscale %s" (string_of_axis x)
      | Text -> sprintf "unset label %i" x
      | Border -> "unset border"
      | Colorbox -> "unset colorbox"
      | Prop -> sprintf "unset %s" x
    in
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



module Quick ( ) = Figure (SVG (Defaults))

module type T = module type of Quick ( )




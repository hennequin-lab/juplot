open Printf
open Owl

type t = out_channel

module Default = struct
  let gnuplot = "gnuplot" (* command name *)
  let file = "/tmp/juplot_tmp.png"
  let term = "pngcairo enhanced transparent font \"Helvetica,10\" size 300,200"
  let defaults = "set key noautotitle; set border 3; set tics out nomirror"
end

module Make (P: module type of Default) = struct
  open P

  (* create a handle *)
  let h =
    let h = Unix.open_process_out gnuplot in
    output_string h (sprintf "set term %s\n" term);
    output_string h (sprintf "set output '%s'\n" file);
    output_string h (defaults ^ "\n");
    h

  let a = ref 0.
  let _ = Gc.finalise (fun _ -> 
      try ignore (Unix.close_process_out h)
      with _ -> ()) a

  let ex cmd = output_string h (cmd^"\n")
  let flush () = flush h
  let close () = ignore (Unix.close_process_out h)
  let output () = 
    ex "unset output";
    flush ();
    close ();
    let ic = open_in_bin file in
    let n = in_channel_length ic in
    let data = really_input_string ic n in
    close_in ic; 
    Jupyter_notebook.display ~base64:true "image/png" data

  let end_signal () = fprintf h "e\n%!"

  let __send_columns m =
    let cols = Array.length m in
    let rows = Array.fold_left max (-1) (Array.map Array.length m) in
    for i=0 to rows-1 do
      for j=0 to cols-1 do
        let mj = m.(j) in
        if i<Array.length mj
        then fprintf h "%f " mj.(i)
        else fprintf h "- ";
      done; fprintf h "\n%!";
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
    let cmds = List.map (fun (_,opts) -> sprintf "'-' %s" opts) data in
    ex ("plot " ^ String.concat ", " cmds);
    List.iter (fun (d,_) -> send_columns (Array.of_list d)) data

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

  let image mat f =
    let n, m = Mat.shape mat in
    List.iter ex [
      sprintf "set xrange [%f:%f]" (-0.5) (float m -. 0.5);
      sprintf "set yrange [%f:%f] reverse" (-0.5) (float n -. 0.5);
      "plot '-' mat w image"];
    send_matrix mat

  let margins = List.iter (function
      | `T x -> ex (sprintf "set tmargin at screen %f" x)
      | `B x -> ex (sprintf "set bmargin at screen %f" x)
      | `L x -> ex (sprintf "set lmargin at screen %f" x)
      | `R x -> ex (sprintf "set rmargin at screen %f" x))

  let multiplot rows cols vspace hspace list f =
    ex (sprintf "set multiplot layout %i,%i" rows cols);
    let plots = Array.of_list list in
    let hrem = 2. *. hspace *. float cols
    and vrem = 2. *. vspace *. float rows in
    let width = (1. -. hrem) /. float cols
    and height = (1. -. vrem) /. float rows in
    for k=0 to Array.length plots - 1 do
      let row = k/cols and col = k mod cols in
      let t = 1. -. hspace -. 2. *. hspace *. float row 
              -. height *. float row in
      let b = t -. height in
      let l = 1. -. (1. -. vspace -. 2. *. vspace *. float col 
                     -. width *. float col) in
      let r = l +. width in
      margins [`T t; `B b; `L l; `R r];
      let () = plots.(k) f in ()
    done;
    ex "unset multiplot"

end


let clear () = Gc.full_major ()

module Quick () = Make (Default)




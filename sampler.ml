type t = {
    k : int;
    p : float array;
    sums : float array;
    stats : bool array;
    mdfy_stack : int Stack.t
}

let make init_f n =
    {
    k = n;
    p = Array.init n init_f;
    sums = Array.init n init_f;
    stats = Array.make n false;
    mdfy_stack = Stack.create()
    }

let get_sum a idx =
    if idx >= a.k then 0.0
    else a.sums.(idx);;

let update_sum a idx =
    let left = idx*2 + 1 in
    let right = left + 1 in
    a.sums.(idx) <- (get_sum a left) +. (get_sum a right) +. a.p.(idx);;

let reinit a =
    let rec for_iter ss =
        if Stack.is_empty ss then ()
        else
            let tmp = Stack.pop ss in
            begin
                a.p.(tmp) <- f tmp;
                a.stats.(tmp) <- false;
                update_sum a tmp;
                for_iter ss
            end
    in for_iter a.mdfy_stack;;

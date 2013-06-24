(*get sums of array a of length k*)
let get_sums a k =
    let sums = Array.copy a in
    let rec for_iter i =
        match i with
        0 -> sums
        | i -> 
            let t = (i-1)/2 in sums.(t) <- sums.(t) +. sums.(i);
            for_iter (i-1)
    in for_iter (k-1);;

let multi_sampler defaults k =
    let avals = Array.copy defaults in
    let sums = get_sums avals k in
    let stats = Array.make (Array.length avals) false in
    let mdfy_stack = Stack.create() in
    let sample_gen () = 
        let r_v = Random.float sums.(0) in
        let rec for_iter i r_v =
            if i >= k then 0
            else
                let new_r = r_v -. avals.(i) in
                let left = i*2 + 1 in
                if new_r <= 0.0 || left >= k then i
                else if new_r <= sums.(left) then for_iter left new_r
                else for_iter (left+1) (new_r-.sums.(left))
        in for_iter 0 r_v
    in let rec to_stack idx =
        if stats.(idx) = true then ()
        else
            stats.(idx) <- true;
            to_stack ((idx-1)/2);
            Stack.push idx mdfy_stack
    in let get_sum idx = 
        if idx >= k then 0.0
        else sums.(idx)
    in let update_sum idx =
        let left = idx*2 + 1 in
        let right = left + 1 in
        sums.(idx) <- (get_sum left) +. (get_sum right) +. avals.(idx)
    in let update_with_stack () =
        Stack.iter update_sum mdfy_stack
    in let to_set idx fval =
        avals.(idx) <- fval;
        to_stack idx
    in let set idx fval = 
        avals.(idx) <- fval;
        let rec deep_set idx = 
            update_sum idx;
            if stats.(idx) = true then deep_set ((idx-1)/2)
            else 
                stats.(idx) <- true;
                deep_set ((idx-1)/2);
                Stack.push idx mdfy_stack
        in deep_set idx
    in sample_gen, set, to_set, update_with_stack;;

let get_sums a k =
    let sums = Array.copy a in
    let rec for_iter i =
        match i with
        0 -> sums
        | i -> 
            let t = (i-1)/2 in sums.(t) <- sums.(t) +. sums.(i);
            for_iter (i-1)
    in for_iter (k-1);;

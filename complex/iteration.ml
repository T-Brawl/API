open Complexe ;;

let f z1 z2 = add (mul z1 z1) z2 ;;

let iteration c m =
   let i = ref 0 
	 and z = ref zero in
	   while norme !z <= 2. && !i <= m do
		 z := f !z c;
		 i := !i + 1
     done ;
		 if !i > m then m else !i;;
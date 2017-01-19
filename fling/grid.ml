type direction = Up | Right | Down | Left

type position = Position.t                                       

type 'a grid = ('a Parray.t) Parray.t

let empty sizex sizey default = Parray.create sizex (Parray.create sizey default)
                             
let get g p =
  Parray.get (Parray.get g (Position.proj_x p)) (Position.proj_y p)

let set g p v =
  Parray.set g (Position.proj_x p)
             (Parray.set (Parray.get g (Position.proj_x p)) (Position.proj_y p) v)

let next_to g p d =
  let dx,dy =
    match d with
    | Up -> (1,0)
    | Right -> (0,1)
    | Down -> (-1, 0)
    | Left -> (0,-1)
  in
  try
    let c = get g (Position.move p (Position.from_int dx dy)) in
    Some c
  with _ -> None

let rows g = Parray.length g

let cols g = Parray.length (Parray.get g 0)

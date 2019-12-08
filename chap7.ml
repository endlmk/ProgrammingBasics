(* 目的: 平面座標から、x軸について対象な座標を返す *)
(* taisho_x: float * float -> float * float *)
let taisho_x point = match point with 
    (x, y) -> (x, -.y)

let%test _ = taisho_x (1., 3.) = (1., -3.)
let%test _ = taisho_x (4., -2.) = (4., 2.)
let%test _ = taisho_x (2., 0.) = (2., 0.)

(* 目的: ２つの平面座標から、その中点を返す *)
(* chuten: float * float -> float * float -> float * float  *)
let chuten point1 point2 = match point1 with
    (x1, y1) -> match point2 with 
        (x2, y2) -> ((x1 +. x2) /. 2., (y1 +. y2) /. 2.)

let%test _ = chuten (1., 2.) (3., 4.) = (2., 3.)
let%test _ = chuten (2., 0.) (3., 0.) = (2.5, 0.)
let%test _ = chuten (1., 2.) (1., 2.) = (1., 2.)
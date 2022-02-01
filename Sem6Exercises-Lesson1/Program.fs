module Lesson1

//1.2
let h (x: float, y:float) =
    sqrt (x**2+y**2)
    
//1.5
let rec f x =
    if x = 0 then 0
    elif x = 1 then 1
    else f(x-1) + f(x-2)
    
//1.6
let rec sum(m,n) =
    if n=0 then m + (m+1) + (m+2)
    else m + (m+(n-1)) + (m+n) + sum(m, n-1)
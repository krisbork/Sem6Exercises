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
    
//2.2
let rec pow(s:string, n:int) =
    if n<1 then ""
    else pow(s, n-1) + s
    
//2.3
let isIthChar(str:string, i, ch) =
    str[i] = ch
    
//2.5
let occInString(str:string, ch) =
    let rec loop i count =
        if i < str.Length then 
            if str[i].ToString().ToLower() = ch.ToString().ToLower() then loop (i+1) (count+1)
            else loop (i+1) count
        else count
    loop 0 0

//Bonus - Case sensitive match
let exactOccInString(str:string, ch) =
    let rec countLoop i count =
        if i < str.Length then 
            if str[i] = ch then countLoop (i+1) (count+1)
            else countLoop (i+1) count
        else count
    countLoop 0 0
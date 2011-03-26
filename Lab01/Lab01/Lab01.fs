open System

let isleap y = (y % 400) = 0 || ((y % 4 = 0) && not (y % 100 = 0)) 

let daysToEndYear y = y * 365 + List.sum [for c in [ for z in 1..y -> isleap z ] do if c = true then yield 1]


let daysToEndMonth (m, y) = 
    let correction (mm, yy) = 
        if mm = 1 then 0
        elif mm > 1 && isleap yy then 1
        else 2
    daysToEndYear (y - 1) + ((367 * m + 5) / 12) - correction (m, y)

let eraDay (d, m, y) = abs (daysToEndMonth (m-1, y) +  d)


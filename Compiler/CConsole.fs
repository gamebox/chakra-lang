module CConsole

let private csi = "\e["

let cursorLeft n = printf "%s%dD" csi n

let cursorMoveToX x = printf "%s%dG" csi x

(*
    Colors
*)

let private blackFg = 30
let private blackBg = 40
let private redFg = 31
let private redBg = 41
let private greenFg = 32
let private greenBg = 42
let private yellowFg = 33
let private yellowBg = 43
let private blueFg = 34
let private blueBg = 44
let private magentaFg = 35
let private magentaBg = 45
let private cyanFg = 36
let private cyanBg = 46
let private whiteFg = 37
let private whiteBg = 47
let private reset = 0 

let private color code = sprintf "\x1b[%im" code

let private withColor code n = sprintf "%s%s%s" (color code) n (color reset)

let red n = withColor redFg n
let green n = withColor greenFg n
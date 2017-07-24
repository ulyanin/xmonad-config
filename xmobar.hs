Config { 
    font = "xft:Dejavu Sans Mono:size=10:regular:antialias=true"
    -- bgColor = "#362b12",
    fgColor = "#fff897",
    bgColor = "#000000",
    -- fgColor = "#ffffff",
    position = Static { xpos = 0, ypos = 0, width = 1920, height = 25 },
    lowerOnStart = True,
    commands = [
         Run Weather "UUDD" ["-t","<tempC>°C","-L","18","-H","25","--normal","green","--high","red","--low","lightblue"] 36000 --moscow
         -- Run Weather "UWKD" ["-t","<station>: <tempC>°C","-L","18","-H","25","--normal","green","--high","red","--low","lightblue"] 36000 -- kazan
        ,Run Memory ["-t","<used>/<total>M (<cache>M)","-H","8192","-L","4096","-h","#FFB6B0","-l","#CEFFAC","-n","#FFFFCC"] 10
        ,Run Network "wlp2s0" [
             "-t"    ,"rx:<rx>, tx:<tx>"
            ,"-H"   ,"200"
            ,"-L"   ,"10"
            ,"-h"   ,"#FFB6B0"
            ,"-l"   ,"#CEFFAC"
            ,"-n"   ,"#FFFFCC"
            , "-c"  , " "
            , "-w"  , "2"
            ] 10
        ,Run Date "%d.%m.%Y %H:%M:%S" "date" 10
        ,Run MultiCpu [ "--template" , "<autototal>"
            , "--Low"      , "50"         -- units: %
            , "--High"     , "85"         -- units: %
            , "--low"      , "gray"
            , "--normal"   , "darkorange"
            , "--high"     , "darkred"
            , "-c"         , " "
            , "-w"         , "3"
        ] 10
        ,Run CoreTemp [ "--template" , "<core0> <core1> <core2>°C"
            , "--Low"      , "70"        -- units: °C
            , "--High"     , "80"        -- units: °C
            , "--low"      , "darkgreen"
            , "--normal"   , "darkorange"
            , "--high"     , "darkred"
        ] 50
        ,Run StdinReader
        ,Run PipeReader "/tmp/.volume-pipe" "vol"
    ],
    sepChar = "%",
    alignSep = "}{",
    template = "%StdinReader% }{ %coretemp% | %multicpu% | %memory%  | %wlp2s0% | %UUDD% | <fc=#FFFFCC>%date%</fc>   "
}

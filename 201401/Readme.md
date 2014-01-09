# Synacor

load ghci

    :l Synacor.hs
    run (load [19,65,0]) [] -- output ascii char 65 then terminate
    -- # "A"

    run (load [20,32768,19,32768,0]) "H"
    -- # "H"

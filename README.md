SmartCheck takes the output of QuickCheck and does data-generic shrinking on it
(you don't have to define shrink methods yourself).  Once it's found the minimal
value it can, it tries to generalize by replacing holes with arbitrary
(well-typed) values.  If for any replacement the test still fails, it
conjectures that you plug anything into the holes.

A typical example
--------------------------------

    > cd SmartCheck/src/
    > ghci -Wall ../examples/MutualRecData.hs
    *MutualRecData> main

    *** Failed! Falsifiable (after 15 tests):  
    M (N (M (N (M (N P (-5) "\GS(") (N (M (N P 35 "$<\rk\183\no\NUL") (N (M (N P 31 "d: \228\244\135\251&") (N P 31 "") 52) 62 "\234\STX\246R\204.P\205") 28) 17 "") (-48)) 29 "7\157[\186X6\242\191*") (N (M (N P (-4) "2f\151\DLE!EG\162/\128") (N (M (N P 2 "n<a\USLRLXK") (N (M (N P 23 "\rK\140\STXQ\b\155( ") (N P 9 "}\247\DLE") 42) (-36) "\SO 1MJL\ETX") (-14)) (-47) "_^") 4) 69 "(\\i") (-48)) (-256) "\vp\179\177\163") (N (M (N P 31 "") (N (M (N (M (N (M (N P (-9) "W\ETXi\DC1\b\201") (N (M (N P (-16) "\150=q") (N (M (N P 5 "\RS") (N P (-10) "]") (-18)) 6 "i\ESC") (-6)) (-16) "\238k") (-55)) (-35) "\NUL\"") (N P 6 "xC2HFW") (-57)) (-21) "\\\187\130\153lI^\EM") (N (M (N (M (N P (-27) "}\DC2") (N (M (N P 26 "P\SYN\ACK\226W8-") (N (M (N (M (N (M (N P (-8) "\199\DLE\144\180") (N (M (N (M (N P 2 "") (N P 1 "c\t") (-3)) 3 "=`") (N (M (N (M (N P (-1) "B") (N (M (N (M (N P 1 "") (N P 0 "") 1) 0 "d") (N P 1 "w") 2) 2 "") 2) (-1) "D\v\163") (N (M (N P (-1) "") (N P 1 "\232M") 0) (-2) "") 0) (-3) "1r[") (-5)) 1 "\DC4\EM") (-4)) 0 "H3\RS\239") (N P 8 "\f\DC1y\151d") 15) 15 "") (N P (-2) "") 28) (-20) "\US\NAKZ\DLEVt") (-10)) (-5) "!M\ACKk\DC4\DC1uY?") (-27)) (-14) "YE+\163\rJVS_4") (N (M (N (M (N (M (N (M (N P (-2) "\237\&4@") (N P 6 "") 6) 2 "^\DLEM\189y") (N P (-16) "\222\247(\DLE") (-14)) (-6) "\DEL\DC1iM\vn\218") (N (M (N P 8 "?\249KH\t\153") (N P 9 "") 26) (-16) "") 30) 14 "\STXOLy[\EOT") (N (M (N P 13 "\249%") (N (M (N (M (N (M (N (M (N P (-1) "x") (N (M (N P (-1) "!b") (N P (-2) "\240") 3) 4 "") 1) 5 "\207") (N (M (N P 4 "") (N (M (N (M (N P (-2) "") (N P (-1) "e") 2) 0 "") (N (M (N (M (N (M (N P 1 "") (N P 1 "") (-1)) 1 "") (N P (-1) "") (-1)) 1 "") (N P (-1) "") (-1)) (-1) "") 4) 3 "\144\b\217\n") 3) 8 "") 1) (-3) "3}`") (N P 3 "6\149") (-11)) (-8) "") (N (M (N (M (N P (-5) "e\222J") (N P 5 "/+G") 6) 1 "\151\151\&9") (N P (-7) "") (-12)) 2 "!\237") 18) (-9) "|R+C\186}c") 6) 14 "a\208\ACK\ENQ\SI!") 55) 10 "\158") (-52)) 14 "") (-45)) 90 "lg") (-12)) 177 "\224I\246\189F%\SOH\DC4-") 187

    *** Smart Shrinking ... 
    *** Smart-shrunk value:
    M (N P 0 "") (N P 1 "") 187

    *** Extrapolating ...
    *** Extrapolated value:
    forall x0 x1:
    M
    |
    +- x1
    |
    +- x0
    |
    `- 187
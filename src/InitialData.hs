{-# LANGUAGE NoImplicitPrelude #-}
module InitialData 
  ( initialBoard
  , initialState
  ) where

import Board
import BState
import Parse
import Validate
import Errors
import Utils
import RIO
import RIO.List ( intercalate )

spaces = intercalate "\n"
    -- Oceans
    ["NAO, Ocean","NWG, Ocean","BAR, Ocean","NTH, Ocean","IRI, Ocean","SKA, Ocean"
    ,"ENG, Ocean","HEL, Ocean","MAO, Ocean","BAL, Ocean","BOT, Ocean","BLA, Ocean"
    ,"WES, Ocean","LYO, Ocean","TYS, Ocean","ION, Ocean","ADR, Ocean","AEG, Ocean","EAS, Ocean"
    ,"Cly, Coast","Edi, Coast [SC]","Lvp, Coast [SC]","Yor, Coast","Wal, Coast","Lon, Coast [SC]" --Eng
     --Gray
    ,"Bel, Coast [SC]","Hol, Coast [SC]","Den, Coast [SC]","Swe, Coast [SC]","Nwy, Coast [SC]"
    ,"Fin, Coast","SpaL, Land","SpaNC, Coast","SpaSC, Coast","BulL, Land","BulEC, Coast","BulSC, Coast"
    ,"Por, Coast [SC]","Naf, Coast","Tun, Coast [SC]","Alb, Coast","Ser, Land [SC]","Gre, Coast [SC]"
    ,"Rum, Coast [SC]"
    ,"StpL, Land","StpNC, Coast","StpSC, Coast","Lvn, Coast","Mos, Land [SC]","War, Land [SC]"
    ,"Ukr, Land","Sev, Coast [SC]" --Rus
    ,"Bre, Coast [SC]","Pic, Coast","Par, Land [SC]","Bur, Land","Gas, Coast","Mar, Coast [SC]"  --Fra
    ,"Kie, Coast [SC]","Ber, Coast [SC]","Pru, Coast","Ruh, Land","Mun, Land [SC]","Sil, Land" --Ger
    ,"Tyr, Land","Boh, Land","Gal, Land","Vie, Land [SC]","Bud, Land [SC]","Tri, Coast [SC]" --Aus
    ,"Pie, Coast","Tus, Coast","Ven, Coast [SC]","Rom, Coast [SC]","Apu, Coast","Nap, Coast [SC]" --Ita
    ,"Arm, Coast","Ank, Coast [SC]","Con, Coast [SC]","Smy, Coast [SC]","Syr, Coast" --Tur
    ]

routes = intercalate "\n" $
  fmap (++ " [F]")
  -- Oceans
    ["NAO-NWG","NAO-Cly","NAO-Lvp","NAO-IRI","NAO-MAO"
    ,"NWG-Cly","NWG-Edi","NWG-NTH","NWG-Nwy","NWG-BAR"
    ,"BAR-Nwy","BAR-StpNC","BOT-StpSC","BOT-Fin","BOT-Swe","BOT-Lvn"
    ,"BAL-Swe","BAL-Den","BAL-Kie","BAL-Ber","BAL-Pru","BAL-Lvn"
    ,"SKA-NTH","SKA-Nwy","SKA-Swe","SKA-Den"
    ,"NTH-Nwy","NTH-Den","NTH-Hol","NTH-Bel","NTH-ENG","NTH-Lon","NTH-Yor","NTH-Edi"
    ,"HEL-Den","HEL-Kie","HEL-Hol"
    ,"ENG-Bel","ENG-Pic","ENG-Bre","ENG-MAO","ENG-IRI","ENG-Wal","ENG-Lon"
    ,"IRI-Lvp","IRI-Wal","IRI-MAO"
    ,"MAO-Bre","MAO-Gas","MAO-SpaNC","MAO-SpaSC","MAO-Por","MAO-WES","MAO-Naf"
    ,"WES-SpaSC","WES-LYO","WES-TYS","WES-Tun","WES-Naf"
    ,"LYO-Mar","LYO-Pie","LYO-Tus","LYO-TYS","LYO-SpaSC"
    ,"TYS-Tus","TYS-Rom","TYS-Nap","TYS-ION","TYS-Tun"
    ,"ADR-Tri","ADR-Alb","ADR-ION","ADR-Apu","ADR-Ven"
    ,"ION-Alb","ION-Gre","ION-AEG","ION-EAS","ION-Nap","ION-Apu"
    ,"AEG-Con","AEG-Smy","AEG-EAS","AEG-Gre","AEG-BulSC"
    ,"BLA-Sev","BLA-Arm","BLA-Ank","BLA-Con","BLA-BulEC","BLA-Rum","EAS-Syr","EAS-Smy"
    --Coasts
    ,"Con-BulEC","Con-BulSC","BulSC-Gre","Nwy-StpNC","StpSC-Fin","Mar-SpaSC","SpaNC-Gas"
    ,"StpSC-Lvn","BulEC-Rum","SpaNC-Por"
    ] 
  ++ fmap (++ " [C]")
      ["BAR-StpL","BOT-StpL","BOT-BAL","NTH-HEL" ,"MAO-SpaL","WES-SpaL","LYO-SpaL","AEG-BulL","BLA-BulL"]
  ++ fmap (++ " [B]")
      ["Cly-Edi","Edi-Yor","Yor-Lon","Lon-Wal","Wal-Lvp","Lvp-Cly" --Eng
      ,"Sev-Arm","Sev-Rum","Lvn-Pru" --Rus
      ,"Arm-Ank","Syr-Smy","Smy-Con","Ank-Con" --Tur
      ,"Tri-Alb","Tri-Ven" --Aus
      ,"Ven-Apu","Apu-Nap","Nap-Rom","Rom-Tus","Tus-Pie","Pie-Mar" --Ita
      ,"Gas-Bre","Bre-Pic","Pic-Bel" --Fra
      ,"Bel-Hol","Hol-Kie","Den-Swe","Den-Kie","Swe-Nwy","Swe-Fin","Gre-Alb","Naf-Tun"--gray
      ,"Kie-Ber","Kie-Pru"--Ger
      ]
  ++ fmap (++ " [A]")
     ["Edi-Lvp","Lvp-Yor","Yor-Wal"-- Eng
     ,"Nwy-StpL","Nwy-Fin","Hol-Ruh","Bel-Ruh","Bel-Bur","Por-SpaL","SpaL-Gas","SpaL-Mar"
     ,"Alb-Ser","Ser-Rum","Ser-BulL","Ser-Gre","Gre-BulL","BulL-Rum","BulL-Con","Rum-Bud"
     ,"Rum-Gal","Rum-Ukr" --gray
      --Rus
     ,"StpL-Mos","StpL-Lvn","StpL-Fin","Mos-Sev","Mos-Ukr","Mos-War","Mos-Lvn"
     ,"Sev-Ukr","Ukr-Gal","Ukr-War","War-Gal","War-Sil","War-Pru","War-Lvn"
     ,"Arm-Syr","Arm-Smy","Smy-Ank"--Tur
      --Aus
     ,"Gal-Bud","Gal-Vie","Gal-Boh","Gal-Sil","Bud-Ser","Bud-Tri","Bud-Vie","Tri-Vie"
     ,"Tri-Tyr","Vie-Tyr","Vie-Boh","Tyr-Ven","Tyr-Pie","Tyr-Mun","Tyr-Boh","Boh-Mun","Boh-Sil"
     ,"Ven-Rom","Ven-Tus","Ven-Pie","Apu-Rom"--Ita
      --Fra
     ,"Bur-Ruh","Bur-Mun","Bur-Mar","Bur-Gas","Bur-Par","Bur-Pic","Mar-Gas","Gas-Par","Par-Bre","Par-Pic"
      --Ger
     ,"Pru-Sil","Sil-Mun","Sil-Ber","Mun-Ber","Mun-Ruh","Mun-Kie","Ruh-Kie"
     ]

areas = intercalate "\n"
  ["Spa: SpaL~SpaNC~SpaSC [SC]","Bul: BulL~BulEC~BulSC [SC]","Stp: StpL~StpNC~StpSC [SC]"]

boardString = "Spaces:\n" ++ spaces ++ "\n\n" 
           ++ "Routes:\n" ++ routes ++ "\n\n"
           ++ "Areas:\n"  ++ areas  ++ "\n"
  
initialBoard = parseValidated parseBoardData boardString >>= uncurry3 mkBoard

occupiedString = intercalate "\n"
  ["Edi, occupied by Eng F","Lvp, occupied by Eng A","Lon, occupied by Eng F"
  ,"Bre, occupied by Fra F","Par, occupied by Fra A","Mar, occupied by Fra A"
  ,"Kie, occupied by Ger F","Ber, occupied by Ger A","Mun, occupied by Ger A"
  ,"Ven, occupied by Ita A","Rom, occupied by Ita A","Nap, occupied by Ita F"
  ,"Tri, occupied by Aus F","Vie, occupied by Aus A","Bud, occupied by Aus A"
  ,"Con, occupied by Tur A","Ank, occupied by Tur F","Smy, occupied by Tur A"
  ,"StpSC, occupied by Rus F","Mos, occupied by Rus A","War, occupied by Rus A","Sev, occupied by Rus F"
  ]
controlledString = intercalate "\n"
  ["Edi, controlled by Eng","Lvp, controlled by Eng","Lon, controlled by Eng"
  ,"Bre, controlled by Fra","Par, controlled by Fra","Mar, controlled by Fra"
  ,"Kie, controlled by Ger","Ber, controlled by Ger","Mun, controlled by Ger"
  ,"Pie, controlled by Ita","Rom, controlled by Ita","Nap, controlled by Ita"
  ,"Tri, controlled by Aus","Vie, controlled by Aus","Bud, controlled by Ita"
  ,"Con, controlled by Tur","Ank, controlled by Tur","Smy, controlled by Tur"
  ,"Stp, controlled by Rus","Mos, controlled by Rus","War, controlled by Rus", "Sev, controlled by Rus"
  ]

stateString = "1901 Spring, status:" ++ "\n\n"
  ++ "Spaces:\n" ++ occupiedString ++ "\n\n"
  ++ "Areas:\n" ++ controlledString ++ "\n"

initialState = do
  board <- initialBoard
  (year, phase, spaceStates, areaStates) <- parseValidated (parseStateData board) stateString 
  mkBState board year phase spaceStates areaStates


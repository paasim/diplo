{-# LANGUAGE NoImplicitPrelude #-}
module InitialData 
  ( initialBoard
  , initialState
  ) where

import Error
import Util
import Board
import BState
import Parse
import Validate
import RIO

spaces = unlines
    -- Oceans
    ["NAO, Ocean","NWG, Ocean","BAR, Ocean","NTH, Ocean","IRI, Ocean","SKA, Ocean"
    ,"ENG, Ocean","HEL, Ocean","MAO, Ocean","BAL, Ocean","BOT, Ocean","BLA, Ocean"
    ,"WES, Ocean","LYO, Ocean","TYS, Ocean","ION, Ocean","ADR, Ocean","AEG, Ocean","EAS, Ocean"
     --Eng
    ,"Cly, Coast","Edi, Coast [SC, England]","Lvp, Coast [SC, England]"
    ,"Yor, Coast","Wal, Coast","Lon, Coast [SC, England]" 
     --Gray
    ,"Bel, Coast [SC, Common]","Hol, Coast [SC, Common]","Den, Coast [SC, Common]"
    ,"Swe, Coast [SC, Common]","Nwy, Coast [SC, Common]","Fin, Coast"
    ,"SpaNC, Coast","SpaSC, Coast","BulEC, Coast","BulSC, Coast"
    ,"Por, Coast [SC, Common]","Naf, Coast","Tun, Coast [SC, Common]","Alb, Coast"
    ,"Ser, Land [SC, Common]","Gre, Coast [SC, Common]","Rum, Coast [SC, Common]"
     --Rus
    ,"StpNC, Coast","StpSC, Coast","Lvn, Coast","Mos, Land [SC, Russia]"
    ,"War, Land [SC, Russia]","Ukr, Land","Sev, Coast [SC, Russia]"
     --Fra
    ,"Bre, Coast [SC, France]","Pic, Coast","Par, Land [SC, France]","Bur, Land"
    ,"Gas, Coast","Mar, Coast [SC, France]"
     --Ger
    ,"Kie, Coast [SC, Germany]","Ber, Coast [SC, Germany]","Pru, Coast","Ruh, Land"
    ,"Mun, Land [SC, Germany]","Sil, Land"
     --Aus
    ,"Tyr, Land","Boh, Land","Gal, Land","Vie, Land [SC, Austria]"
    ,"Bud, Land [SC, Austria]","Tri, Coast [SC, Austria]"
     --Ita
    ,"Pie, Coast","Tus, Coast","Ven, Coast [SC, Italy]","Rom, Coast [SC, Italy]"
    ,"Apu, Coast","Nap, Coast [SC, Italy]"
     --Tur
    ,"Arm, Coast","Ank, Coast [SC, Turkey]","Con, Coast [SC, Turkey]"
    ,"Smy, Coast [SC, Turkey]","Syr, Coast"
    ]

-- foldmap?
routes = unlines $
  fmap (<> " [F]")
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
    ] 
  <> fmap (<> " [C]")
      ["BAR-StpSC","BOT-StpNC","BOT-BAL","NTH-HEL","WES-SpaNC"
      ,"LYO-SpaNC","AEG-BulEC","BLA-BulSC"]
  <> fmap (<> " [B]")
      ["Cly-Edi","Edi-Yor","Yor-Lon","Lon-Wal","Wal-Lvp","Lvp-Cly" --Eng
      ,"Sev-Arm","Sev-Rum","Lvn-Pru","StpNC-Nwy","StpSC-Lvn","StpSC-Fin" --Rus
      ,"Arm-Ank","Syr-Smy","Smy-Con","Ank-Con" --Tur
      ,"Tri-Alb","Tri-Ven" --Aus
      ,"Ven-Apu","Apu-Nap","Nap-Rom","Rom-Tus","Tus-Pie","Pie-Mar" --Ita
      ,"Mar-SpaSC","Gas-SpaNC","Gas-Bre","Bre-Pic","Pic-Bel" --Fra
      ,"Bel-Hol","Hol-Kie","Den-Swe","Den-Kie","Swe-Nwy","Swe-Fin","Gre-Alb"
      ,"BulEC-Rum","BulEC-Con","BulSC-Con","BulSC-Gre","Naf-Tun","SpaNC-Por","SpaSC-Por"--gray
      ,"Kie-Ber","Kie-Pru"--Ger
      ]
  <> fmap (<> " [A]")
     ["Edi-Lvp","Lvp-Yor","Yor-Wal"-- Eng
     ,"Nwy-StpSC","Nwy-Fin","Hol-Ruh","Bel-Ruh","Bel-Bur","SpaSC-Gas","SpaNC-Mar"
     ,"Alb-Ser","Ser-Rum","Ser-BulEC","Ser-BulSC","Ser-Gre","BulSC-Rum","BulEC-Gre"
     ,"Rum-Bud","Rum-Gal","Rum-Ukr" --gray
      --Rus
     ,"StpNC-Mos","StpSC-Mos","StpNC-Lvn","StpNC-Fin","Mos-Sev","Mos-Ukr","Mos-War","Mos-Lvn"
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

areas = unlines
  ["Spa: SpaNC~SpaSC [SC, Common]","Bul: BulEC~BulSC [SC, Common]"
  ,"Stp: StpNC~StpSC [SC, Russia]"]

boardString = "Spaces:\n" <> spaces <> "\n"
           <> "Routes:\n" <> routes <> "\n"
           <> "Areas:\n"  <> areas
  
initialBoard = parseValidated parseBoardData boardString >>= uncurry3 mkBoard

occupiedString = unlines
  ["Edi, occupied by Eng F","Lvp, occupied by Eng A","Lon, occupied by Eng F"
  ,"Bre, occupied by Fra F","Par, occupied by Fra A","Mar, occupied by Fra A"
  ,"Kie, occupied by Ger F","Ber, occupied by Ger A","Mun, occupied by Ger A"
  ,"Ven, occupied by Ita A","Rom, occupied by Ita A","Nap, occupied by Ita F"
  ,"Tri, occupied by Aus F","Vie, occupied by Aus A","Bud, occupied by Aus A"
  ,"Con, occupied by Tur A","Ank, occupied by Tur F","Smy, occupied by Tur A"
  ,"StpSC, occupied by Rus F","Mos, occupied by Rus A","War, occupied by Rus A","Sev, occupied by Rus F"
  ]
controlledString = unlines
  ["Edi, controlled by Eng","Lvp, controlled by Eng","Lon, controlled by Eng"
  ,"Bre, controlled by Fra","Par, controlled by Fra","Mar, controlled by Fra"
  ,"Kie, controlled by Ger","Ber, controlled by Ger","Mun, controlled by Ger"
  ,"Ven, controlled by Ita","Rom, controlled by Ita","Nap, controlled by Ita"
  ,"Tri, controlled by Aus","Vie, controlled by Aus","Bud, controlled by Aus"
  ,"Con, controlled by Tur","Ank, controlled by Tur","Smy, controlled by Tur"
  ,"Stp, controlled by Rus","Mos, controlled by Rus","War, controlled by Rus", "Sev, controlled by Rus"
  ]

dislodgedString = unlines []

stateString = "Spring 1901, status:" <> "\n\n"
  <> "Spaces:\n" <> occupiedString <> "\n"
  <> "Areas:\n" <> controlledString <> "\n"
  <> "Dislodged units:\n" <> dislodgedString

initialState = do
  board <- initialBoard
  (year, phase, spaceStates, areaStates) <- parseValidated (parseStateData board) stateString 
  mkBState board year phase spaceStates areaStates


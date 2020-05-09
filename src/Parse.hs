{-# LANGUAGE NoImplicitPrelude #-}
module Parse 
  ( parseValidated
  , parseBoardData
  , parseStateData
  , parseOrder
  , parseOrders
  , parseValidatedFromFile
  ) where

import Orders
import Spaces
import Units
import BoardDefs
import StateDefs
import Errors
import RIO
import qualified RIO.Set as S ( filter, fromList, member, null, singleton, toList )
import qualified RIO.Map as M ( filter, keys )
import qualified RIO.NonEmpty as NE ( last )
import Text.Trifecta 
import Text.Trifecta.Parser
import Text.Parser.Combinators
import Text.Parser.Token

-- Parsing
-- utils
resultToValidated :: Show a => Result a -> Validated a
resultToValidated r = case r of
  (Success a) -> Valid a
  (Failure f) -> ParsingError f

parseValidated :: Show a => Parser a -> String -> Validated a
parseValidated p str = resultToValidated $ parseString p mempty str

parseValidatedFromFile :: (Show a, MonadIO m) => Parser a -> String -> m (Validated a)
parseValidatedFromFile p = fmap resultToValidated . parseFromFileEx p

withSpace :: Parser a -> Parser a
withSpace p = p <* space

-- general
parseName :: Parser String
parseName = some letter

parseLand = string "Land" *> return Land
parseOcean = string "Ocean" *> return Ocean
parseCoast = string "Coast" *> return Coast
parseAnySpaceType = choice [parseLand, parseOcean, parseCoast]

parseArmy  = (string "Army"  <|> string "A") *> return Army
parseFleet = (string "Fleet" <|> string "F") *> return Fleet
parseAnyUnitType = parseArmy <|> parseFleet

parseAus = (string "Austria" <|> string "Aus") *> return Austria
parseEng = (string "England" <|> string "Eng") *> return England
parseFra = (string "France"  <|> string "Fra") *> return France
parseGer = (string "Germany" <|> string "Ger") *> return Germany
parseIta = (string "Italy"   <|> string "Ita") *> return Italy
parseRus = (string "Russia"  <|> string "Rus") *> return Russia
parseTur = (string "Turkey"  <|> string "Tur") *> return Turkey
parseAnyCountry = choice [parseAus, parseEng, parseFra, parseGer, parseIta, parseRus, parseTur]

parseARoute = char 'A' *> return ArmyOnly
parseFRoute = char 'F' *> return FleetOnly
parseBRoute = char 'B' *> return BothUnits
parseCRoute = char 'C' *> return ConvoyOnly
parseRouteType :: Parser RouteType
parseRouteType = string " [" *> choice [parseARoute, parseFRoute, parseBRoute, parseCRoute] <* char ']'

parseUnitWith :: (Parser UnitType) -> Parser Unit
parseUnitWith utp = do
  uc <- withSpace parseAnyCountry
  ut <- utp
  return $ Unit uc ut

parseAnyUnit = parseUnitWith parseAnyUnitType
parseSpecifiedUnit Army = parseUnitWith parseArmy
parseSpecifiedUnit Fleet = parseUnitWith parseFleet
 
toANameParser :: (a -> String) -> a -> Parser a
toANameParser showA a = string (showA a) *> return a

spaceToSpaceNameParser = toANameParser spaceName
areaToAreaNameParser   = toANameParser areaName


-- Order parsing
-- SpaceFilter is for filtering spaces that are valid when parsing
data SpaceFilter = SpaceFilter { runSpaceFilter :: Space -> Bool }

instance Semigroup SpaceFilter where
  SpaceFilter f1 <> SpaceFilter f2 = SpaceFilter (\spc -> f1 spc && f2 spc)

instance Monoid SpaceFilter where
  mempty = SpaceFilter (const True)

spaceTypeFilter :: SpaceType -> SpaceFilter
spaceTypeFilter st = SpaceFilter (\spc -> spaceType spc == st)

unitTypeFilter :: UnitType -> SpaceFilter
unitTypeFilter Army  = SpaceFilter $ \spc -> spaceType spc /= Ocean
unitTypeFilter Fleet = SpaceFilter $ \spc -> spaceType spc /= Land

routeForUnitType Army  ArmyOnly  = True
routeForUnitType Fleet FleetOnly = True
routeForUnitType _     BothUnits = True
routeForUnitType _     _         = False

routeFilter :: Space -> (RouteType -> Bool) -> Board -> SpaceFilter
routeFilter spc' f b = SpaceFilter $
  \spc -> not . S.null . S.filter (\x -> x == (Route spc' spc BothUnits) && (f . routeType) x) $ boardRoutes b

anyRouteFilter :: Space -> Board -> SpaceFilter
anyRouteFilter spc' b = SpaceFilter $ \spc -> S.member (Route spc' spc BothUnits) (boardRoutes b)

parseSpaceWith :: SpaceFilter -> Board -> Parser Space
parseSpaceWith (SpaceFilter f) = choice . fmap spaceToSpaceNameParser . S.toList . S.filter f . boardSpaces

parseUnitAndSpaceWith :: (Parser Unit) -> Board -> GameState -> Parser (Unit, Space)
parseUnitAndSpaceWith up b gs = do
  unit <- withSpace up
  spc <- choice . fmap spaceToSpaceNameParser . M.keys . M.filter (== unit) . occupiers $ gs
  return (unit, spc)

parseHold :: Board -> GameState -> Unit -> Space -> Parser OrderData
parseHold b gs unit spc = string " holds" *> return Hold

parseAttack :: Board -> GameState -> Unit -> Space -> Parser OrderData
parseAttack b gs attacker spcFrom = do
  string " to " <|> string "-"
  spcTo <- parseSpaceWith (routeFilter spcFrom (routeForUnitType (unitType attacker)) b) b
  return $ Attack spcTo

parseVias :: Board -> Space -> Parser [Space]
parseVias b viaPrev = do
  testParse <- optional $ withSpace (parseSpaceWith (spaceTypeFilter Ocean <> anyRouteFilter viaPrev b) b)
  case testParse of
    Nothing        -> return []
    Just (viaNext) -> ((:) viaNext) <$> parseVias b viaNext

parseAttackViaConvoy :: Board -> GameState -> Unit -> Space -> Parser OrderData
parseAttackViaConvoy b gs attacker spcFrom = do
  string " via "
  spcVia <- withSpace $ parseSpaceWith (routeFilter spcFrom (/= ArmyOnly) b) b
  spcVias <- parseVias b spcVia
  string "to "
  spcTo <- parseSpaceWith (routeFilter (NE.last $ spcVia :| spcVias) (/= ArmyOnly) b) b
  return $ AttackViaConvoy (ConvoyPath (spcVia :| spcVias) spcTo)

parseConvoy :: Board -> GameState -> Unit -> Space -> Parser OrderData
parseConvoy b gs convoyer convoyerSpace = do
  string " convoys "
  (convoyee, convoyeeFrom) <- parseUnitAndSpaceWith (parseSpecifiedUnit Army) b gs
  string " to "
  convoyeeTo <- parseSpaceWith (unitTypeFilter Army) b
  return $ Convoy convoyee convoyeeFrom convoyeeTo

parseSuppAttack :: Board -> Unit -> Space -> Unit -> Space -> Parser OrderData
parseSuppAttack b supporter supporterSpace attacker spcFrom = do
  string " to " <|> string "-"
  spcTo <- parseSpaceWith (routeFilter spcFrom (routeForUnitType (unitType attacker)) b
                        <> routeFilter supporterSpace (routeForUnitType (unitType supporter)) b) b
  return $ SuppAttack attacker spcFrom spcTo

parseSuppHold :: Board -> Unit -> Space -> Unit -> Space -> Parser OrderData
parseSuppHold b supporter supporterSpace holder holdAt = do
  if (runSpaceFilter (routeFilter supporterSpace (routeForUnitType (unitType supporter)) b)) holdAt
    then string " holds" >> return (SuppHold holder holdAt)
    else unexpected $ "No route between '" ++ show supporterSpace ++ "' and '" ++ show holdAt ++ "'"

parseSupport :: Board -> GameState -> Unit -> Space -> Parser OrderData
parseSupport b gs supporter supporterSpace = do
  string " supports ("
  (supportee, supporteeSpace) <- parseUnitAndSpaceWith parseAnyUnit b gs
  choice [ parseSuppHold   b supporter supporterSpace supportee supporteeSpace
         , parseSuppAttack b supporter supporterSpace supportee supporteeSpace] <* char ')'

parseOrderData :: Board -> GameState -> Unit -> Space -> Parser OrderData
parseOrderData board gs unit spc = choice . fmap (\parser -> parser board gs unit spc)
  $ [parseHold, parseAttack, parseSupport, parseConvoy, parseAttackViaConvoy]

parseOrder :: Board -> GameState -> Parser Order
parseOrder b gs = do
  (unit, spc) <- parseUnitAndSpaceWith parseAnyUnit b gs
  orderData <- parseOrderData b gs unit spc
  return $ Order unit spc orderData

parseOrders :: Board -> GameState -> Parser [Order]
parseOrders b gs = sepEndBy1 (parseOrder b gs) newline <* eof

-- board
parseNewSpace :: Parser (Space, Bool)
parseNewSpace = do
  spcName <- parseName
  withSpace $ char ','
  spcType <- parseAnySpaceType
  supplyStatus <- parseSupplyStatus
  return (Space spcName spcType, supplyStatus)

parseNewRouteData :: Parser Space -> Parser (Space, Space, RouteType)
parseNewRouteData sp = do
  spcFrom <- sp
  char '-'
  spcTo <- sp
  rType <- parseRouteType
  return (spcFrom, spcTo, rType)

parseSupplyStatus :: Parser Bool
parseSupplyStatus = optional (string " [SC]") >>= return . isJust


parseTrivialArea :: Parser Space -> Parser Area
parseTrivialArea sp = do
  spc <- sp
  return $ Area (spaceName spc) (S.singleton spc) True

parseNonTrivialArea :: Parser Space -> Parser Area
parseNonTrivialArea sp = do
  areaName <- parseName
  withSpace $ char ':'
  spcs <- sepBy1 sp (char '~') 
  return $ Area (areaName) (S.fromList spcs) False

parseArea :: Parser Space -> Parser (Area, Bool)
parseArea sp = do
  area <- parseTrivialArea sp <|> parseNonTrivialArea sp
  supplyStatus <- parseSupplyStatus
  return (area, supplyStatus)

parseBoardData :: Parser ([(Space, Bool)], [(Space, Space, RouteType)], [(Area, Bool)])
parseBoardData = do
  string "Spaces:\n"
  spcs <- sepEndBy1 parseNewSpace newline
  string "\nRoutes:\n"
  routes <- sepEndBy1 (parseNewRouteData . choice . fmap (spaceToSpaceNameParser . fst) $ spcs) newline
  string "\nAreas:\n"
  areas <- sepEndBy1 (parseArea . choice . fmap (spaceToSpaceNameParser . fst) $ spcs) newline <* eof
  return (spcs, routes, areas)

-- state
parseSpring = string "Spring" *> return Spring
parseFall = string "Fall" *> return Fall
parsePhase = parseSpring <|> parseFall

parseMaybeB :: String -> String -> Parser b -> Parser (Maybe b)
parseMaybeB strNothing strJust p = choice [ string strNothing >> return Nothing
                                          , string strJust >> space >> p >>= (return . Just) ]

parseStateFor :: Parser b -> String -> Parser a -> Parser (a, Maybe b)
parseStateFor bParser str aParser = do
  a <- aParser
  comma
  mb <- parseMaybeB ("un" ++ str) (str ++ " by") bParser
  return (a, mb)

parseSpaceStates :: Board -> Parser (Space, Maybe Unit)
parseSpaceStates = parseStateFor parseAnyUnit "occupied" . choice . fmap spaceToSpaceNameParser . S.toList . boardSpaces

parseAreaStates :: Board -> Parser (Area, Maybe Country)
parseAreaStates = parseStateFor parseAnyCountry "controlled" . choice . fmap areaToAreaNameParser . S.toList . boardAreas
 
parseStateData :: Board -> Parser (Int, Phase, [(Space, Maybe Unit)], [(Area, Maybe Country)])
parseStateData b = do
  year <- integer
  phase <- parsePhase
  string ", status:\n\n"
  string "Spaces:\n"
  spaceStates <- sepEndBy1 (parseSpaceStates b) newline
  string "\nAreas:\n"
  areaStates <- sepEndBy1 (parseAreaStates b) newline <* eof
  return (fromIntegral year, phase, spaceStates, areaStates)
  

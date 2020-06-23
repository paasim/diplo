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
import Board
import BState
import Errors
import RIO
import qualified RIO.List as L ( delete, notElem, nub )
import qualified RIO.Map as M ( elems, filter, keys, lookup, member )
import qualified RIO.Set as S ( filter, fromList, singleton, toList )
import qualified RIO.NonEmpty as NE ( last )
import Text.Trifecta 
import qualified Text.Parser.Combinators as Comb ( try )

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

-- units
parseSpecificUnitType :: UnitType -> Parser UnitType
parseSpecificUnitType Army  = (string "Army"  <|> string "A") *> return Army
parseSpecificUnitType Fleet = (string "Fleet" <|> string "F") *> return Fleet

parseAnyUnitType :: Parser UnitType
parseAnyUnitType = choice . fmap parseSpecificUnitType $ [Army, Fleet]

parseSpecificCountry :: Country -> Parser Country
parseSpecificCountry Austria = (string "Austria" <|> string "Aus") *> return Austria
parseSpecificCountry England = (string "England" <|> string "Eng") *> return England
parseSpecificCountry France  = (string "France"  <|> string "Fra") *> return France
parseSpecificCountry Germany = (string "Germany" <|> string "Ger") *> return Germany
parseSpecificCountry Italy   = (string "Italy"   <|> string "Ita") *> return Italy
parseSpecificCountry Russia  = (string "Russia"  <|> string "Rus") *> return Russia
parseSpecificCountry Turkey  = (string "Turkey"  <|> string "Tur") *> return Turkey

parseAnyCountry :: Parser Country
parseAnyCountry = choice . fmap parseSpecificCountry $ [Austria, England, France, Germany, Italy, Russia, Turkey]

parseAnyUnit :: Parser Unit
parseAnyUnit = Unit <$> (parseAnyCountry <* space) <*> parseAnyUnitType

-- UnitFilter is for filtering units that exist on the board (ie. occupy a space)
newtype UnitFilter = UnitFilter { runUnitFilter :: Unit -> Bool }

instance Semigroup UnitFilter where
  UnitFilter f1 <> UnitFilter f2 = UnitFilter (\spc -> f1 spc && f2 spc)

instance Monoid UnitFilter where
  mempty = UnitFilter (const True)

unitToUnitParser :: Unit -> Parser Unit
unitToUnitParser (Unit uc ut) = Unit <$> (parseSpecificCountry uc <* space) <*> parseSpecificUnitType ut

unitTypeFilter :: UnitType -> UnitFilter
unitTypeFilter ut = UnitFilter $ \unit -> unitType unit == ut

existingUnits :: BState -> [Unit]
existingUnits = L.nub . M.elems . occupiers

parseUnitWith :: UnitFilter -> BState -> Parser Unit
parseUnitWith (UnitFilter f) = choice . fmap (Comb.try . unitToUnitParser) . filter f . existingUnits

-- spaces
parseName :: Parser String
parseName = some letter

parseSpecificSpaceType :: SpaceType -> Parser SpaceType
parseSpecificSpaceType Land  = string "Land"  *> return Land
parseSpecificSpaceType Ocean = string "Ocean" *> return Ocean
parseSpecificSpaceType Coast = string "Coast" *> return Coast

parseAnySpaceType :: Parser SpaceType
parseAnySpaceType = choice . fmap parseSpecificSpaceType $ [Land, Ocean, Coast]

parseSpecificRoute :: RouteType -> Parser RouteType
parseSpecificRoute ArmyOnly   = char 'A' *> return ArmyOnly
parseSpecificRoute FleetOnly  = char 'F' *> return FleetOnly
parseSpecificRoute BothUnits  = char 'B' *> return BothUnits
parseSpecificRoute ConvoyOnly = char 'C' *> return ConvoyOnly

parseAnyRouteType :: Parser RouteType
parseAnyRouteType = choice . fmap parseSpecificRoute $ [ArmyOnly, FleetOnly, BothUnits, ConvoyOnly]

toANameParser :: (a -> String) -> a -> Parser a
toANameParser showA a = string (showA a) *> return a

spaceToSpaceNameParser :: Space -> Parser Space
spaceToSpaceNameParser = toANameParser spaceName

areaToAreaNameParser :: Area -> Parser Area 
areaToAreaNameParser = toANameParser areaName

parseExistingUnitAndSpace :: BState -> UnitFilter -> Parser (Unit, Space)
parseExistingUnitAndSpace state uf = do
  unit <- parseUnitWith uf state <* space
  spc <- choice . fmap spaceToSpaceNameParser . M.keys . M.filter (== unit) . occupiers $ state
  return (unit, spc)

parseSpaceWith :: Board -> SpaceFilter -> Parser Space
parseSpaceWith board (SpaceFilter f) = choice . fmap spaceToSpaceNameParser . S.toList . S.filter f . boardSpaces $ board

-- SpaceFilter is for filtering spaces that are valid when parsing
newtype SpaceFilter = SpaceFilter { runSpaceFilter :: Space -> Bool }

instance Semigroup SpaceFilter where
  SpaceFilter f1 <> SpaceFilter f2 = SpaceFilter (\spc -> f1 spc && f2 spc)

instance Monoid SpaceFilter where
  mempty = SpaceFilter (const True)

spaceTypeFilter :: SpaceType -> SpaceFilter
spaceTypeFilter st = SpaceFilter $ \spc -> spaceType spc == st

unitTypeSpaceFilter :: UnitType -> SpaceFilter
unitTypeSpaceFilter Army = SpaceFilter $ \spc -> spaceType spc == Land || spaceType spc == Coast
unitTypeSpaceFilter Fleet = SpaceFilter $ \spc -> spaceType spc /= Land

occupiedBy :: BState -> Unit -> SpaceFilter
occupiedBy state unit1 = SpaceFilter $ \spc -> case M.lookup spc (occupiers state) of
  Just unit2 -> unit1 == unit2
  Nothing    -> False

occupied :: BState -> SpaceFilter
occupied state = SpaceFilter $ \spc -> M.member spc (occupiers state)

notIn :: [Space] -> SpaceFilter
notIn spcs = SpaceFilter $ \spc -> L.notElem spc spcs

-- route
routeForUnitType :: UnitType -> RouteType -> Bool
routeForUnitType Army  ArmyOnly  = True
routeForUnitType Fleet FleetOnly = True
routeForUnitType _     BothUnits = True
routeForUnitType _     _         = False

convoyRoute :: RouteType -> Bool
convoyRoute rt = rt == FleetOnly || rt == ConvoyOnly

routeExists :: Board -> Space -> Space -> (RouteType -> Bool) -> Bool
routeExists board spc1 spc2 validRoute = maybe False validRoute (M.lookup (Route spc1 spc2) . boardRoutes $ board)

regularRouteExists' :: Board -> Space -> Space -> Unit -> Bool
regularRouteExists' board spc1 spc2 unit = routeExists board spc1 spc2 (routeForUnitType (unitType unit))

convoyRouteExists' :: Board -> Space -> Space -> Bool
convoyRouteExists' board spc1 spc2 = routeExists board spc1 spc2 convoyRoute

regularRouteExists :: Board -> Space -> Unit -> SpaceFilter
regularRouteExists board spc1 unit = SpaceFilter $ \spc2 -> regularRouteExists' board spc1 spc2 unit

convoyRouteExists :: Board -> Space -> SpaceFilter
convoyRouteExists board spc1 = SpaceFilter $ \spc2 -> convoyRouteExists' board spc1 spc2

convoyPathFilter :: Board -> BState -> [Space] -> Space -> SpaceFilter
convoyPathFilter board state prevs spcFrom = spaceTypeFilter Ocean
  <> convoyRouteExists board spcFrom
  <> occupied state
  <> notIn prevs


-- phase
parseSpecificPhase :: Phase -> Parser Phase
parseSpecificPhase Spring = string "Spring" *> return Spring
parseSpecificPhase Fall   = string "Fall"   *> return Fall

parseAnyPhase :: Parser Phase
parseAnyPhase = choice . fmap parseSpecificPhase $ [Spring, Fall]


-- orders
parseHold :: Board -> BState -> Unit -> Space -> Parser OrderData
parseHold _ _ _ _ = string " holds" *> return Hold

parseAttackStart :: Parser String
parseAttackStart = (string " to" *> string " ") <|> string "-"

parseAttack :: Board -> BState -> Unit -> Space -> Parser OrderData
parseAttack board _ attacker attackFrom = do
  parseAttackStart
  attackTo <- parseSpaceWith board (regularRouteExists board attackFrom attacker)
  return $ Attack attackTo

parseSuppHold :: Board -> BState -> Unit -> Space -> Parser OrderData
parseSuppHold board state supporter supporterAt = do
  holder <- parseUnitWith mempty state <* space
  holderAt <- parseSpaceWith board $ regularRouteExists board supporterAt supporter <> occupiedBy state holder
  string " holds"
  return $ SuppHold holder holderAt

parseSuppAttack :: Board -> BState -> Unit -> Space -> Parser OrderData
parseSuppAttack board state supporter supporterFrom = do
  (attacker, attackFrom) <- parseExistingUnitAndSpace state mempty 
  parseAttackStart
  attackTo <- parseSpaceWith board $
    regularRouteExists board attackFrom attacker <> regularRouteExists board supporterFrom supporter
  return $ SuppAttack attacker attackFrom attackTo

parseSupport :: Board -> BState -> Unit -> Space -> Parser OrderData
parseSupport board state supporter supporterSpace = do
  string " supports" *> string " ("
  choice [ Comb.try $ parseSuppHold   board state supporter supporterSpace
         , Comb.try $ parseSuppAttack board state supporter supporterSpace ] <* char ')'

parseVias :: Board -> BState -> [Space] -> Space -> Parser [Space]
parseVias board state prevs current = do
  testParse <- optional $ parseSpaceWith board (convoyPathFilter board state prevs current) <* space
  case testParse of
    Nothing      -> return . reverse $ current:prevs
    Just viaNext -> parseVias board state (current:prevs) viaNext

parseAttackViaConvoy :: Board -> BState -> Unit -> Space -> Parser OrderData
parseAttackViaConvoy board state attacker attackerFrom = do
  string " via" *> space
  spcVia <- parseSpaceWith board (convoyPathFilter board state [] attackerFrom) <* space
  spcVias <- parseVias board state [attackerFrom] spcVia
  string "to" *> space
  spcTo <- parseSpaceWith board $ convoyRouteExists board (NE.last $ spcVia :| spcVias)
  return $ AttackViaConvoy (ConvoyPath (spcVia :| spcVias) spcTo)

parseConvoy :: Board -> BState -> Unit -> Space -> Parser OrderData
parseConvoy board state convoyer convoyerSpace = do
  string " convoys" *> space
  (convoyee, convoyeeFrom) <- parseExistingUnitAndSpace state (unitTypeFilter Army)
  space *> string "to" *> space
  convoyeeTo <- parseSpaceWith board $ unitTypeSpaceFilter Army
  return $ Convoy convoyee convoyeeFrom convoyeeTo

parseOrderData :: Board -> BState -> Unit -> Space -> Parser OrderData
parseOrderData board state unit spc =
  choice . fmap (\parser -> parser board state unit spc) $ parserList where
    parserList = case unitType unit of 
      Army  -> [parseHold, parseAttack, parseSupport, parseAttackViaConvoy]
      Fleet -> [parseHold, parseAttack, parseSupport, parseConvoy]

parseOrder :: Board -> BState -> Parser Order
parseOrder board state = do
  (unit, spc) <- parseExistingUnitAndSpace state mempty
  orderData <- parseOrderData board state unit spc
  return $ Order unit spc orderData

parseOrders :: Board -> BState -> Parser [Order]
parseOrders board state = sepEndBy1 (parseOrder board state) newline <* eof


-- board
parseSupplyStatus :: Parser Bool
parseSupplyStatus = optional (string " [SC]") >>= return . isJust

parseNewSpace :: Parser (Space, Bool)
parseNewSpace = do
  spcName <- parseName
  char ',' *> space
  spcType <- parseAnySpaceType
  supplyStatus <- parseSupplyStatus
  return (Space spcName spcType, supplyStatus)

parseTrivialArea :: Parser Space -> Parser Area
parseTrivialArea sp = do
  spc <- sp
  return $ Area (spaceName spc) (S.singleton spc) True

parseNonTrivialArea :: Parser Space -> Parser Area
parseNonTrivialArea sp = do
  areaName <- parseName
  char ':' *> space
  spcs <- sepBy1 sp (char '~') 
  return $ Area areaName (S.fromList spcs) False

parseArea :: Parser Space -> Parser (Area, Bool)
parseArea sp = do
  area <- parseTrivialArea sp <|> parseNonTrivialArea sp
  supplyStatus <- parseSupplyStatus
  return (area, supplyStatus)

parseNewRoute :: [Space] -> Parser (Route, RouteType)
parseNewRoute spcs = do
  spcFrom <- choice . fmap spaceToSpaceNameParser $ spcs
  char '-'
  spcTo <- choice . fmap spaceToSpaceNameParser . L.delete spcFrom $ spcs
  rType <- space *> char '[' *> parseAnyRouteType <* char ']'
  return (Route spcFrom spcTo, rType)

parseBoardData :: Parser ([(Space, Bool)], [(Route, RouteType)], [(Area, Bool)])
parseBoardData = do
  string "Spaces:\n"
  spcs <- sepEndBy1 parseNewSpace newline
  string "\nRoutes:\n"
  routes <- sepEndBy1 (parseNewRoute . fmap fst $ spcs) newline
  string "\nAreas:\n"
  areas <- sepEndBy1 (parseArea . choice . fmap (spaceToSpaceNameParser . fst) $ spcs) newline <* eof
  return (spcs, routes, areas)


-- state
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
parseStateData board = do
  year <- integer
  phase <- parseAnyPhase
  string ", status:\n\n"
  string "Spaces:\n"
  spaceStates <- sepEndBy1 (parseSpaceStates board) newline
  string "\nAreas:\n"
  areaStates <- sepEndBy1 (parseAreaStates board) newline <* eof
  return (fromIntegral year, phase, spaceStates, areaStates)
  

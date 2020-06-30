{-# LANGUAGE NoImplicitPrelude #-}
module Parse 
  ( parseValidated
  , parseBoardData
  , parseStateData
  , parseOrder
  , parseOrders
  , parseValidatedFromFile
  , parseRetreatOrder
  , parseRetreatOrders
  , parseBuildOrder
  , parseBuildOrders
  ) where

import Order
import Space
import Unit
import Board
import BState
import Error
import Util
import RIO
import qualified RIO.List as L ( (\\), delete, elem, notElem, partition )
import qualified RIO.Map as M ( elems, filter, keys, lookup, member, toList )
import qualified RIO.Set as S ( filter, fromList, map, member, singleton, toList )
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

parseCommonSupplyOrigin :: Parser SupplyOrigin
parseCommonSupplyOrigin = string "Common" *> return Common

parseAnySupplyOrigin :: Parser SupplyOrigin
parseAnySupplyOrigin = fmap HomeSupply parseAnyCountry <|> parseCommonSupplyOrigin

parseAnyUnit :: Parser Unit
parseAnyUnit = Unit <$> (parseAnyCountry <* space) <*> parseAnyUnitType

-- for parsing units that match certain criteria
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
existingUnits = getUniques . M.elems . occupiers

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
parseAnyRouteType = choice . fmap parseSpecificRoute
                  $ [ArmyOnly, FleetOnly, BothUnits, ConvoyOnly]

toANameParser :: (a -> String) -> a -> Parser a
toANameParser showA a = string (showA a) *> return a

spaceToSpaceNameParser :: Space -> Parser Space
spaceToSpaceNameParser = toANameParser spaceName

-- parse areaName given an area
areaToAreaNameParser :: Area -> Parser Area 
areaToAreaNameParser = toANameParser areaName

parseSpaceWith :: Board -> SpaceFilter -> Parser Space
parseSpaceWith board (SpaceFilter f) = choice . fmap spaceToSpaceNameParser
                                     . filter f . S.toList . boardSpaces $ board

-- for parsing spaces that match given criteria
newtype SpaceFilter = SpaceFilter { runSpaceFilter :: Space -> Bool }

instance Semigroup SpaceFilter where
  SpaceFilter f1 <> SpaceFilter f2 = SpaceFilter (\spc -> f1 spc && f2 spc)

instance Monoid SpaceFilter where
  mempty = SpaceFilter (const True)

spaceTypeFilter :: SpaceType -> SpaceFilter
spaceTypeFilter st = SpaceFilter $ \spc -> spaceType spc == st

unitTypeSpaceFilter :: UnitType -> SpaceFilter
unitTypeSpaceFilter Army = SpaceFilter $
  \spc -> spaceType spc == Land || spaceType spc == Coast
unitTypeSpaceFilter Fleet = SpaceFilter $ \spc -> spaceType spc /= Land

occupiedBy :: BState -> Unit -> SpaceFilter
occupiedBy state unit1 = SpaceFilter $ \spc -> case M.lookup spc (occupiers state) of
  Just unit2 -> unit1 == unit2
  Nothing    -> False

occupied :: BState -> SpaceFilter
occupied state = SpaceFilter (\spc -> M.member spc . occupiers $ state)

unOccupied :: BState -> SpaceFilter
unOccupied state = SpaceFilter (\spc -> not . M.member spc . occupiers $ state)

spcNotIn :: [Space] -> SpaceFilter
spcNotIn spcs = SpaceFilter $ \spc -> L.notElem spc spcs

spcIn :: [Space] -> SpaceFilter
spcIn spcs = SpaceFilter $ \spc -> L.elem spc spcs

controlledSpaces :: Country -> BState -> [Space]
controlledSpaces c = M.keys . M.filter (== c) . controllers >=> S.toList . areaMembers

isControlled :: Country -> BState -> SpaceFilter
isControlled country = spcIn . controlledSpaces country

countryHomeSCs :: Country -> BState -> [Space]
countryHomeSCs c = M.keys . M.filter (HomeSupply c ==)
                 . boardSupplyCenters . gameBoard >=> S.toList . areaMembers

isHomeSupply :: Country -> BState -> SpaceFilter
isHomeSupply country = spcIn . countryHomeSCs country

countryOccupiesSpace :: Country -> BState -> SpaceFilter
countryOccupiesSpace country =
  spcIn . M.keys . M.filter ((==) country . unitCountry) . occupiers

-- spaces that the country can build new units at
buildableSpaceFilter :: BState -> Country -> SpaceFilter
buildableSpaceFilter state country = unOccupied state
                                   <> isHomeSupply country state
                                   <> isControlled country state

-- filter for spaces at which a hold can be supported (for the given supporter and holder type)
supportableHoldFilter :: BState -> Space -> Unit -> Unit -> SpaceFilter
supportableHoldFilter state supporterAt supporter holder = 
  routeForUnitExists (gameBoard state) supporterAt supporter <> occupiedBy state holder

supportableAttackFilter :: BState -> Space -> Unit -> Space -> Unit -> SpaceFilter
supportableAttackFilter state attackFrom attacker supporterFrom supporter =
     routeForUnitExists (gameBoard state) attackFrom attacker
  <> routeForUnitExists (gameBoard state) supporterFrom supporter

-- existing as in the unit occupies a given space
parseExistingUnitAndSpace :: BState -> UnitFilter -> Parser (Unit, Space)
parseExistingUnitAndSpace state uf = do
  unit <- parseUnitWith uf state <* space
  spc <- parseSpaceWith (gameBoard state) (occupiedBy state unit)
  return (unit, spc)


-- route
routeForUnitType :: UnitType -> RouteType -> Bool
routeForUnitType Army  ArmyOnly  = True
routeForUnitType Fleet FleetOnly = True
routeForUnitType _     BothUnits = True
routeForUnitType _     _         = False

convoyRoute :: RouteType -> Bool
convoyRoute rt = rt == FleetOnly || rt == ConvoyOnly

routeWithTypeExists :: Board -> Space -> Space -> (RouteType -> Bool) -> Bool
routeWithTypeExists board spc1 spc2 validRoute =
  maybe False validRoute (M.lookup (Route spc1 spc2) . boardRoutes $ board)

regularRouteExists :: Board -> Space -> SpaceFilter
regularRouteExists board spc1 = SpaceFilter $
  \spc2 -> routeWithTypeExists board spc1 spc2 (ConvoyOnly /=)

routeForUnitExists :: Board -> Space -> Unit -> SpaceFilter
routeForUnitExists board spc1 unit = SpaceFilter $
  \spc2 -> routeWithTypeExists board spc1 spc2 (routeForUnitType (unitType unit))

convoyRouteExists :: Board -> Space -> SpaceFilter
convoyRouteExists board spc1 = SpaceFilter $ \spc2 -> routeWithTypeExists board spc1 spc2 convoyRoute

convoyPathFilter :: BState -> [Space] -> Space -> SpaceFilter
convoyPathFilter state prevs spcFrom = spaceTypeFilter Ocean
  <> convoyRouteExists (gameBoard state) spcFrom
  <> occupied state
  <> spcNotIn prevs


-- phase
parseSpecificSeason :: Season -> Parser Season
parseSpecificSeason Spring = string "Spring" *> return Spring
parseSpecificSeason Fall   = string "Fall"   *> return Fall
parseSpecificSeason Winter   = string "Winter"   *> return Winter

parseAnySeason :: Parser Season
parseAnySeason = choice . fmap parseSpecificSeason $ [Spring, Fall, Winter]

parseAnyPhase :: Parser Phase
parseAnyPhase = Phase <$> (parseAnySeason <* space) <*> (fromIntegral <$> integer)

-- board
parseSupplyStatus :: Parser (Maybe SupplyOrigin)
parseSupplyStatus = optional (string " [SC, " *> parseAnySupplyOrigin <* string "]")

parseNewSpace :: Parser (Space, Maybe SupplyOrigin)
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

parseArea :: Parser Space -> Parser (Area, Maybe SupplyOrigin)
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

parseBoardData :: Parser ([(Space, Maybe SupplyOrigin)], [(Route, RouteType)], [(Area, Maybe SupplyOrigin)])
parseBoardData = do
  string "Spaces:\n"
  spcs <- sepEndBy1 parseNewSpace newline
  string "\nRoutes:\n"
  routes <- sepEndBy1 (parseNewRoute . fmap fst $ spcs) newline
  string "\nAreas:\n"
  areas <- sepEndBy1 (parseArea . choice . fmap (spaceToSpaceNameParser . fst) $ spcs) newline
  optional newline <* eof -- allow one extra newline at the end
  return (spcs, routes, areas)


-- state
parseMaybeB :: String -> String -> Parser b -> Parser (Maybe b)
parseMaybeB strNothing strJust p = choice [ string strNothing >> return Nothing
                                          , string strJust >> space >> p >>= (return . Just) ]

parseStateFor :: Parser b -> String -> Parser a -> Parser (a, Maybe b)
parseStateFor bParser str aParser = do
  a <- aParser
  comma
  mb <- parseMaybeB ("un" <> str) (str <> " by") bParser
  return (a, mb)

parseSpaceStates :: Board -> Parser (Space, Maybe Unit)
parseSpaceStates = parseStateFor parseAnyUnit "occupied" . choice
                 . fmap spaceToSpaceNameParser . S.toList . boardSpaces

parseAreaStates :: Board -> Parser (Area, Maybe Country)
parseAreaStates = parseStateFor parseAnyCountry "controlled" . choice
                . fmap areaToAreaNameParser . S.toList . boardAreas
 
parseDislodgedUnit :: Board -> Parser DislodgedUnit
parseDislodgedUnit board = do
  unit <- parseAnyUnit
  string " at "
  spcAt <- parseSpaceWith board (unitTypeSpaceFilter (unitType unit))
  string ", dislodged from "
  spcFrom <- parseSpaceWith board (regularRouteExists board spcAt)
  return $ DislodgedUnit unit spcAt spcFrom

parseStateData :: Board -> Parser (Phase, [(Space, Maybe Unit)], [(Area, Maybe Country)], [DislodgedUnit])
parseStateData board = do
  phase <- parseAnyPhase
  string ", status:\n"
  string "\nSpaces:\n"
  spaceStates <- sepEndBy1 (parseSpaceStates board) newline
  string "\nAreas:\n"
  areaStates <- sepEndBy1 (parseAreaStates board) newline
  string "\nDislodged units:\n"
  dislodgedUnits <- sepEndBy (parseDislodgedUnit board) newline
  optional newline <* optional newline <* eof -- allow 1-2 extra newlines at the end
  return (phase, spaceStates, areaStates, dislodgedUnits)
  

-- regular orders
parseHold :: BState -> Unit -> Space -> Parser OrderData
parseHold _ _ _ = string " holds" *> return Hold

parseAttackStart :: Parser String
parseAttackStart = (string " to" *> string " ") <|> string "-"

parseAttack :: BState -> Unit -> Space -> Parser OrderData
parseAttack state attacker attackFrom = do
  parseAttackStart
  attackTo <- parseSpaceWith (gameBoard state) (routeForUnitExists (gameBoard state) attackFrom attacker)
  return $ Attack attackTo

parseSuppHold :: BState -> Unit -> Space -> Parser OrderData
parseSuppHold state supporter supporterAt = do
  holder <- parseUnitWith mempty state <* space
  holderAt <- parseSpaceWith (gameBoard state) (supportableHoldFilter state supporterAt supporter holder)
  string " holds"
  return $ SuppHold holder holderAt

parseSuppAttack :: BState -> Unit -> Space -> Parser OrderData
parseSuppAttack state supporter supporterFrom = do
  (attacker, attackFrom) <- parseExistingUnitAndSpace state mempty 
  parseAttackStart
  attackTo <- parseSpaceWith (gameBoard state)
                             (supportableAttackFilter state attackFrom attacker supporterFrom supporter)
  return $ SuppAttack attacker attackFrom attackTo

parseSupport :: BState -> Unit -> Space -> Parser OrderData
parseSupport state supporter supporterSpace = do
  string " supports" *> string " ("
  choice [ Comb.try $ parseSuppHold   state supporter supporterSpace
         , Comb.try $ parseSuppAttack state supporter supporterSpace ] <* char ')'

parseVias :: BState -> [Space] -> Space -> Parser [Space]
parseVias state prevs current = do
  testParse <- optional $ parseSpaceWith (gameBoard state) (convoyPathFilter state prevs current) <* space
  case testParse of
    Nothing      -> return . reverse $ current:prevs
    Just viaNext -> parseVias state (current:prevs) viaNext

parseAttackViaConvoy :: BState -> Unit -> Space -> Parser OrderData
parseAttackViaConvoy state attacker attackerFrom = do
  string " via" *> space
  spcVia <- parseSpaceWith (gameBoard state) (convoyPathFilter state [] attackerFrom) <* space
  spcVias <- parseVias state [attackerFrom] spcVia
  string "to" *> space
  spcTo <- parseSpaceWith (gameBoard state) (convoyRouteExists (gameBoard state) (NE.last $ spcVia :| spcVias))
  return $ AttackViaConvoy (ConvoyPath (spcVia :| spcVias) spcTo)

parseConvoy :: BState -> Unit -> Space -> Parser OrderData
parseConvoy state convoyer convoyerSpace = do
  string " convoys" *> space
  (convoyee, convoyeeFrom) <- parseExistingUnitAndSpace state (unitTypeFilter Army)
  space *> string "to" *> space
  convoyeeTo <- parseSpaceWith (gameBoard state) (unitTypeSpaceFilter Army)
  return $ Convoy convoyee convoyeeFrom convoyeeTo

parseOrderData :: BState -> Unit -> Space -> Parser OrderData
parseOrderData state unit spc =
  choice . fmap (\parser -> parser state unit spc) $ parserList where
    parserList = case unitType unit of 
      Army  -> [parseHold, parseAttack, parseSupport, parseAttackViaConvoy]
      Fleet -> [parseHold, parseAttack, parseSupport, parseConvoy]

parseOrder :: BState -> Parser Order
parseOrder state = do
  (unit, spc) <- parseExistingUnitAndSpace state mempty
  orderData <- parseOrderData state unit spc
  return $ Order unit spc orderData

parseOrders :: BState -> Parser [Order]
parseOrders state = sepEndBy1 (parseOrder state) newline <* eof


-- retreat orders
dislodgedUnitParser :: Set DislodgedUnit -> Parser Unit
dislodgedUnitParser = choice . fmap (Comb.try . unitToUnitParser) . S.toList . S.map dislodgedUnit

filterDislodgedUnit :: Unit -> Set DislodgedUnit -> Set DislodgedUnit
filterDislodgedUnit unit = S.filter ((==) unit . dislodgedUnit)

dislodgedAtFilter :: Set DislodgedUnit -> Unit -> SpaceFilter
dislodgedAtFilter dus unit = SpaceFilter
  (\spc -> S.member spc . S.map dislodgedAt . filterDislodgedUnit unit $ dus)

filterDislodgedAt :: Space -> Set DislodgedUnit -> Set DislodgedUnit
filterDislodgedAt spc = S.filter ((==) spc . dislodgedAt)

dislodgedFromFilter :: Set DislodgedUnit -> Space -> SpaceFilter
dislodgedFromFilter dus spc1 = SpaceFilter
  (\spc2 -> not . S.member spc2 . S.map dislodgedFrom . filterDislodgedAt spc1 $ dus)

retreatRouteFilter :: BState -> Space -> Unit -> SpaceFilter
retreatRouteFilter state spc unit = unOccupied state
                                      <> routeForUnitExists (gameBoard state) spc unit
                                      <> dislodgedFromFilter (dislodgedUnits state) spc

parseRetreat :: BState -> Unit -> Space -> Parser RetreatOrder
parseRetreat state unit spc = do
  string "retreats to "
  RORetreat unit spc <$> parseSpaceWith (gameBoard state) (retreatRouteFilter state spc unit)

parseDisband :: Unit -> Space -> Parser RetreatOrder
parseDisband unit spc = string "disbands" *> return (RODisband unit spc)

parseRetreatOrder :: BState -> Parser RetreatOrder
parseRetreatOrder state = do
  unit <- dislodgedUnitParser (dislodgedUnits state) <* space
  dislodgedAt <- parseSpaceWith (gameBoard state) (dislodgedAtFilter (dislodgedUnits state) unit)
  space
  choice . fmap (\parser -> parser unit dislodgedAt) $ [ parseDisband, parseRetreat state ]

parseRetreatOrders :: BState -> Parser [RetreatOrder]
parseRetreatOrders state = sepEndBy1 (parseRetreatOrder state) newline <* eof


-- build/disband orders
unitTypeForSpaceType :: SpaceType -> [UnitType]
unitTypeForSpaceType Land = [Army]
unitTypeForSpaceType Ocean = [Fleet]
unitTypeForSpaceType Coast = [Army,Fleet]


parseBOBuild :: BState -> Country -> Parser BuildOrder
parseBOBuild state country' = do
  country <- parseSpecificCountry country'
  string " builds "
  spc <- parseSpaceWith (gameBoard state) (buildableSpaceFilter state country)
  space
  ut <- choice . fmap parseSpecificUnitType . unitTypeForSpaceType . spaceType $ spc
  return $ BOBuild country spc ut

parseBODisband :: BState -> Country -> Parser BuildOrder
parseBODisband state country' = do
  country <- parseSpecificCountry country'
  string " disbands "
  spc <- parseSpaceWith (gameBoard state) (countryOccupiesSpace country state)
  return $ BODisband country spc

getBOParser :: BState -> Country -> Int -> Maybe (Parser BuildOrder)
getBOParser state c i = case compare 0 i of
  LT -> Just (parseBOBuild state c)
  EQ -> Nothing -- cannot build or disband
  GT -> Just (parseBODisband state c)

parseBuildOrder :: BState -> Parser BuildOrder
parseBuildOrder state = 
  choice . catMaybes . fmap (uncurry (getBOParser state)) . M.toList . unitDifference $ state

parseBuildOrders :: BState -> Parser [BuildOrder]
parseBuildOrders state = sepEndBy (parseBuildOrder state) newline <* eof


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
import Province
import Unit
import Board
import BState
import Error
import Util
import RIO
import qualified RIO.List as L
import qualified RIO.Map as M
import qualified RIO.Set as S
import qualified RIO.NonEmpty as NE
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
  UnitFilter f1 <> UnitFilter f2 = UnitFilter (\prov -> f1 prov && f2 prov)

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

-- province
parseName :: Parser String
parseName = some letter

parseSpecificProvinceType :: ProvinceType -> Parser ProvinceType
parseSpecificProvinceType Land  = string "Land"  *> return Land
parseSpecificProvinceType Ocean = string "Ocean" *> return Ocean
parseSpecificProvinceType Coast = string "Coast" *> return Coast

parseAnyProvinceType :: Parser ProvinceType
parseAnyProvinceType = choice . fmap parseSpecificProvinceType $ [Land, Ocean, Coast]

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

provinceToProvinceNameParser :: Province -> Parser Province
provinceToProvinceNameParser = toANameParser provinceName

-- parse areaName given an area
areaToAreaNameParser :: Area -> Parser Area 
areaToAreaNameParser = toANameParser show

parseProvinceWith :: Board -> ProvinceFilter -> Parser Province
parseProvinceWith board (ProvinceFilter f) = choice . fmap provinceToProvinceNameParser
                                     . filter f . S.toList . boardProvinces $ board

-- for parsing provinces that match given criteria
newtype ProvinceFilter = ProvinceFilter { runProvinceFilter :: Province -> Bool }

instance Semigroup ProvinceFilter where
  ProvinceFilter f1 <> ProvinceFilter f2 = ProvinceFilter (\prov -> f1 prov && f2 prov)

instance Monoid ProvinceFilter where
  mempty = ProvinceFilter (const True)

provinceTypeFilter :: ProvinceType -> ProvinceFilter
provinceTypeFilter st = ProvinceFilter $ \prov -> provinceType prov == st

unitTypeProvinceFilter :: UnitType -> ProvinceFilter
unitTypeProvinceFilter Army = ProvinceFilter $
  \prov -> provinceType prov == Land || provinceType prov == Coast
unitTypeProvinceFilter Fleet = ProvinceFilter $ \prov -> provinceType prov /= Land

occupiedBy :: BState -> Unit -> ProvinceFilter
occupiedBy state unit1 = ProvinceFilter $ \prov -> case M.lookup prov (occupiers state) of
  Just unit2 -> unit1 == unit2
  Nothing    -> False

occupied :: BState -> ProvinceFilter
occupied state = ProvinceFilter (\prov -> M.member prov . occupiers $ state)

unOccupied :: BState -> ProvinceFilter
unOccupied state = ProvinceFilter (\prov -> not . M.member prov . occupiers $ state)

provNotIn :: [Province] -> ProvinceFilter
provNotIn provs = ProvinceFilter $ \prov -> L.notElem prov provs

provIn :: [Province] -> ProvinceFilter
provIn provs = ProvinceFilter $ \prov -> L.elem prov provs

controlledProvinces :: Country -> BState -> [Province]
controlledProvinces c = join . fmap (NE.toList . areaProvinces . fst)
                      . filter ((==) c . snd) . M.toList . controllers

isControlled :: Country -> BState -> ProvinceFilter
isControlled country = provIn . controlledProvinces country

countryHomeSCs :: Country -> BState -> [Province]
countryHomeSCs c = fmap fst . filter ((==) (HomeSupply c) . snd)
                 . M.toList . boardSupplyCenters . gameBoard

isHomeSupply :: Country -> BState -> ProvinceFilter
isHomeSupply country = provIn . countryHomeSCs country

countryOccupiesProvince :: Country -> BState -> ProvinceFilter
countryOccupiesProvince country =
  provIn . M.keys . M.filter ((==) country . unitCountry) . occupiers

-- provinces that the country can build new units at
buildableProvinceFilter :: BState -> Country -> ProvinceFilter
buildableProvinceFilter state country = unOccupied state
                                   <> isHomeSupply country state
                                   <> isControlled country state

-- filter for provinces at which a hold can be supported (for the given supporter and holder type)
supportableHoldFilter :: BState -> Province -> Unit -> Unit -> ProvinceFilter
supportableHoldFilter state supporterAt supporter holder = 
  routeForUnitExists (gameBoard state) supporterAt supporter <> occupiedBy state holder

supportableAttackFilter :: BState -> Province -> Unit -> Province -> Unit -> ProvinceFilter
supportableAttackFilter state attackFrom attacker supporterFrom supporter =
     routeForUnitExists (gameBoard state) attackFrom attacker
  <> routeForUnitExists (gameBoard state) supporterFrom supporter

-- existing as in the unit occupies a given province
parseExistingUnitAndProvince :: BState -> UnitFilter -> Parser (Unit, Province)
parseExistingUnitAndProvince state uf = do
  unit <- parseUnitWith uf state <* space
  prov <- parseProvinceWith (gameBoard state) (occupiedBy state unit)
  return (unit, prov)


-- route
routeForUnitType :: UnitType -> RouteType -> Bool
routeForUnitType Army  ArmyOnly  = True
routeForUnitType Fleet FleetOnly = True
routeForUnitType _     BothUnits = True
routeForUnitType _     _         = False

convoyRoute :: RouteType -> Bool
convoyRoute rt = rt == FleetOnly || rt == ConvoyOnly

routeWithTypeExists :: Board -> Province -> Province -> (RouteType -> Bool) -> Bool
routeWithTypeExists board prov1 prov2 validRoute =
  maybe False validRoute (M.lookup (Route prov1 prov2) . boardRoutes $ board)

regularRouteExists :: Board -> Province -> ProvinceFilter
regularRouteExists board prov1 = ProvinceFilter $
  \prov2 -> routeWithTypeExists board prov1 prov2 (ConvoyOnly /=)

routeForUnitExists :: Board -> Province -> Unit -> ProvinceFilter
routeForUnitExists board prov1 unit = ProvinceFilter $
  \prov2 -> routeWithTypeExists board prov1 prov2 (routeForUnitType (unitType unit))

convoyRouteExists :: Board -> Province -> ProvinceFilter
convoyRouteExists board prov1 = ProvinceFilter $ \prov2 -> routeWithTypeExists board prov1 prov2 convoyRoute

convoyPathFilter :: BState -> [Province] -> Province -> ProvinceFilter
convoyPathFilter state prevs provFrom = provinceTypeFilter Ocean
  <> convoyRouteExists (gameBoard state) provFrom
  <> occupied state
  <> provNotIn prevs


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

parseNewProvince :: Parser (Province, Maybe SupplyOrigin)
parseNewProvince = do
  provName <- parseName
  char ',' *> space
  provType <- parseAnyProvinceType
  supplyStatus <- parseSupplyStatus
  return (Province provName provType, supplyStatus)

parseMultipleProvinces :: [Province] -> [Province] -> Parser [Province]
parseMultipleProvinces notParsed parsed = do
  prov <- choice . fmap provinceToProvinceNameParser $ notParsed
  isNext <- optional $ char '~'
  case isNext of
    Nothing  -> return $ prov:parsed
    (Just _) -> parseMultipleProvinces (filter (prov /=) notParsed) (prov:parsed)

parseArea :: [Province] -> Parser ([Province], Area)
parseArea notParsed = do 
  areaName <- parseName
  string ": "
  prov <- choice . fmap provinceToProvinceNameParser $ notParsed
  char '~'
  provs <- parseMultipleProvinces (filter (prov /=) notParsed) []
  return (prov:provs, Area areaName (prov :| provs))

parseAreas :: [Province] -> Parser [Area]
parseAreas notParsed = do
  (parsed, area) <- parseArea notParsed
  newline
  isLast <- optional newline
  case isLast of
    Nothing  -> (area :) <$> parseAreas (notParsed L.\\ parsed)
    (Just _) -> return [area]

parseNewRoute :: [Province] -> Parser (Route, RouteType)
parseNewRoute provs = do
  provFrom <- choice . fmap provinceToProvinceNameParser $ provs
  char '-'
  provTo <- choice . fmap provinceToProvinceNameParser . L.delete provFrom $ provs
  rType <- space *> char '[' *> parseAnyRouteType <* char ']'
  return (Route provFrom provTo, rType)

parseBoardData :: Parser ([(Province, Maybe SupplyOrigin)], [(Route, RouteType)], [Area])
parseBoardData = do
  string "Provinces:\n"
  provs <- sepEndBy1 parseNewProvince newline
  string "\nRoutes:\n"
  routes <- sepEndBy1 (parseNewRoute . fmap fst $ provs) newline
  string "\nAreas:\n"
  areas <- parseAreas . fmap fst $ provs
  optional newline <* eof -- allow one extra newline at the end
  return (provs, routes, areas)


-- state
parseStateFor :: Parser b -> String -> Parser a -> Parser (a, b)
parseStateFor bParser separator aParser = do
  a <- aParser
  string ", " *> string separator <* string " by "
  b <- bParser
  return (a, b)

parseProvinceStates :: Board -> Parser (Province, Unit)
parseProvinceStates = parseStateFor parseAnyUnit "occupied"
                    . choice
                    . fmap provinceToProvinceNameParser . S.toList . boardProvinces

parseAreaStates :: Board -> Parser (Area, Country)
parseAreaStates board = parseStateFor parseAnyCountry "controlled"
                      . choice
                      . fmap areaToAreaNameParser . S.toList . S.map (toArea board)
                      . boardProvinces $ board
 
parseDislodgedUnit :: Board -> Parser DislodgedUnit
parseDislodgedUnit board = do
  unit <- parseAnyUnit
  string " at "
  provAt <- parseProvinceWith board (unitTypeProvinceFilter (unitType unit))
  string ", dislodged from "
  provFrom <- parseProvinceWith board (regularRouteExists board provAt)
  return $ DislodgedUnit unit provAt provFrom

parseStateData :: Board -> Parser (Phase, [(Province, Unit)], [(Area, Country)], [DislodgedUnit])
parseStateData board = do
  phase <- parseAnyPhase
  string ", status:\n"
  string "\nProvinces:\n"
  provinceStates <- sepEndBy1 (parseProvinceStates board) newline
  string "\nAreas:\n"
  areaStates <- sepEndBy1 (parseAreaStates board) newline
  string "\nDislodged units:\n"
  dislodgedUnits <- sepEndBy (parseDislodgedUnit board) newline
  optional newline <* eof -- allow 1-2 extra newlines at the end
  return (phase, provinceStates, areaStates, dislodgedUnits)
  

-- regular orders
parseHold :: BState -> Unit -> Province -> Parser OrderData
parseHold _ _ _ = string " holds" *> return Hold

parseAttackStart :: Parser String
parseAttackStart = (string " to" *> string " ") <|> string "-"

parseAttack :: BState -> Unit -> Province -> Parser OrderData
parseAttack state attacker attackFrom = do
  parseAttackStart
  attackTo <- parseProvinceWith (gameBoard state) (routeForUnitExists (gameBoard state) attackFrom attacker)
  return $ Attack attackTo

parseSuppHold :: BState -> Unit -> Province -> Parser OrderData
parseSuppHold state supporter supporterAt = do
  holder <- parseUnitWith mempty state <* space
  holderAt <- parseProvinceWith (gameBoard state) (supportableHoldFilter state supporterAt supporter holder)
  string " holds"
  return $ SuppHold holder holderAt

parseSuppAttack :: BState -> Unit -> Province -> Parser OrderData
parseSuppAttack state supporter supporterFrom = do
  (attacker, attackFrom) <- parseExistingUnitAndProvince state mempty 
  parseAttackStart
  attackTo <- parseProvinceWith (gameBoard state)
                             (supportableAttackFilter state attackFrom attacker supporterFrom supporter)
  return $ SuppAttack attacker attackFrom attackTo

parseSupport :: BState -> Unit -> Province -> Parser OrderData
parseSupport state supporter supporterProvince = do
  string " supports" *> string " ("
  choice [ Comb.try $ parseSuppHold   state supporter supporterProvince
         , Comb.try $ parseSuppAttack state supporter supporterProvince ] <* char ')'

parseVias :: BState -> [Province] -> Province -> Parser [Province]
parseVias state prevs current = do
  testParse <- optional $ parseProvinceWith (gameBoard state) (convoyPathFilter state prevs current) <* space
  case testParse of
    Nothing      -> return . reverse $ current:prevs
    Just viaNext -> parseVias state (current:prevs) viaNext

parseAttackViaConvoy :: BState -> Unit -> Province -> Parser OrderData
parseAttackViaConvoy state attacker attackerFrom = do
  string " via" *> space
  provVia <- parseProvinceWith (gameBoard state) (convoyPathFilter state [] attackerFrom) <* space
  provVias <- parseVias state [attackerFrom] provVia
  string "to" *> space
  provTo <- parseProvinceWith (gameBoard state) (convoyRouteExists (gameBoard state) (NE.last $ provVia :| provVias))
  return $ AttackViaConvoy (ConvoyPath (provVia :| provVias) provTo)

parseConvoy :: BState -> Unit -> Province -> Parser OrderData
parseConvoy state convoyer convoyerProvince = do
  string " convoys" *> space
  (convoyee, convoyeeFrom) <- parseExistingUnitAndProvince state (unitTypeFilter Army)
  space *> string "to" *> space
  convoyeeTo <- parseProvinceWith (gameBoard state) (unitTypeProvinceFilter Army)
  return $ Convoy convoyee convoyeeFrom convoyeeTo

parseOrderData :: BState -> Unit -> Province -> Parser OrderData
parseOrderData state unit prov =
  choice . fmap (\parser -> parser state unit prov) $ parserList where
    parserList = case unitType unit of 
      Army  -> [parseHold, parseAttack, parseSupport, parseAttackViaConvoy]
      Fleet -> [parseHold, parseAttack, parseSupport, parseConvoy]

parseOrder :: BState -> Parser Order
parseOrder state = do
  (unit, prov) <- parseExistingUnitAndProvince state mempty
  orderData <- parseOrderData state unit prov
  return $ Order unit prov orderData

parseOrders :: BState -> Parser [Order]
parseOrders state = sepEndBy1 (parseOrder state) newline <* eof


-- retreat orders
dislodgedUnitParser :: Set DislodgedUnit -> Parser Unit
dislodgedUnitParser = choice . fmap (Comb.try . unitToUnitParser) . S.toList . S.map dislodgedUnit

filterDislodgedUnit :: Unit -> Set DislodgedUnit -> Set DislodgedUnit
filterDislodgedUnit unit = S.filter ((==) unit . dislodgedUnit)

dislodgedAtFilter :: Set DislodgedUnit -> Unit -> ProvinceFilter
dislodgedAtFilter dus unit = ProvinceFilter
  (\prov -> S.member prov . S.map dislodgedAt . filterDislodgedUnit unit $ dus)

filterDislodgedAt :: Province -> Set DislodgedUnit -> Set DislodgedUnit
filterDislodgedAt prov = S.filter ((==) prov . dislodgedAt)

dislodgedFromFilter :: Set DislodgedUnit -> Province -> ProvinceFilter
dislodgedFromFilter dus prov1 = ProvinceFilter
  (\prov2 -> not . S.member prov2 . S.map dislodgedFrom . filterDislodgedAt prov1 $ dus)

retreatRouteFilter :: BState -> Province -> Unit -> ProvinceFilter
retreatRouteFilter state prov unit = unOccupied state
                                      <> routeForUnitExists (gameBoard state) prov unit
                                      <> dislodgedFromFilter (dislodgedUnits state) prov

parseRetreat :: BState -> Unit -> Province -> Parser RetreatOrder
parseRetreat state unit prov = do
  string "retreats to "
  RORetreat unit prov <$> parseProvinceWith (gameBoard state) (retreatRouteFilter state prov unit)

parseDisband :: Unit -> Province -> Parser RetreatOrder
parseDisband unit prov = string "disbands" *> return (RODisband unit prov)

parseRetreatOrder :: BState -> Parser RetreatOrder
parseRetreatOrder state = do
  unit <- dislodgedUnitParser (dislodgedUnits state) <* space
  dislodgedAt <- parseProvinceWith (gameBoard state) (dislodgedAtFilter (dislodgedUnits state) unit)
  space
  choice . fmap (\parser -> parser unit dislodgedAt) $ [ parseDisband, parseRetreat state ]

parseRetreatOrders :: BState -> Parser [RetreatOrder]
parseRetreatOrders state = sepEndBy1 (parseRetreatOrder state) newline <* eof


-- build/disband orders
unitTypeForProvinceType :: ProvinceType -> [UnitType]
unitTypeForProvinceType Land = [Army]
unitTypeForProvinceType Ocean = [Fleet]
unitTypeForProvinceType Coast = [Army,Fleet]


parseBOBuild :: BState -> Country -> Parser BuildOrder
parseBOBuild state country' = do
  country <- parseSpecificCountry country'
  string " builds "
  prov <- parseProvinceWith (gameBoard state) (buildableProvinceFilter state country)
  space
  ut <- choice . fmap parseSpecificUnitType . unitTypeForProvinceType . provinceType $ prov
  return $ BOBuild country prov ut

parseBODisband :: BState -> Country -> Parser BuildOrder
parseBODisband state country' = do
  country <- parseSpecificCountry country'
  string " disbands "
  prov <- parseProvinceWith (gameBoard state) (countryOccupiesProvince country state)
  return $ BODisband country prov

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


{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Awaywox where

import Data.Maybe
import Control.Monad
import Control.Monad.State
import Text.XML.Light
import Network.HTTP.Conduit
import qualified Data.ByteString.Char8 as BS
import Control.Concurrent
import System.Exit

data Killmail = Killmail {
    killID :: Int
{-
  , solarSystemID :: Int
  , killTime :: String
  , moonID :: Int
  , victim :: Toon
-}
  , attackers :: [Toon]
{-
  , _stringValue :: String
-}
} deriving (Show, Eq)

data Toon = Toon {
    shipTypeID :: Int
{-
  , damageTaken :: Maybe Int
  , factionName :: String
-}
  , factionID :: Int
{-
  , allianceName :: String
  , allianceID :: Int
  , corporationName :: String
-}
  , characterName :: String
{-
  , characterID :: Int
  , securityStatus :: Maybe Int
  , damageDone :: Maybe Int
  , finalBlow :: Maybe Int
  , weaponTypeID :: Maybe Int
-}
} deriving (Show, Eq)

xmlemToKillmails :: Element -> Maybe [Killmail]
xmlemToKillmails e = do
    resultsElem <- results e
    rowsetElem <- rowset resultsElem
    killElems <- return $ kills rowsetElem
    forM killElems makeKillmail

results = findChild (unqual "result")
rowset = findChild (unqual "rowset")

kills :: Element -> [Element]
kills e = findChildren (unqual "row") e

makeKillmail :: Element -> Maybe Killmail
makeKillmail e = do
    kid <- lookupAttr (unqual "killID") (elAttribs e) >>= readMaybe
    arowset <- findChild (unqual "rowset") e
    as <- makeAttackers arowset
    return $ Killmail {
        killID = kid
      , attackers = as
    } 

makeAttackers rowset = forM (findChildren (unqual "row") rowset) makeAttacker
makeAttacker (Element _ as _ _) = do
    stid <- lookupAttr (unqual "shipTypeID") as >>= readMaybe
    fid <- lookupAttr (unqual "factionID") as >>= readMaybe
    cn <- lookupAttr (unqual "characterName") as
    return Toon {
        shipTypeID = stid
      , factionID = fid
      , characterName = cn
    }

isFriend :: Toon -> Bool
isFriend (Toon _ 500001 _) = True
isFriend (Toon _ 500003 _) = True
isFriend _ = False

getTraitors :: Killmail -> [(Int, Toon)]
getTraitors (Killmail id as) = map (id,) $ filter isFriend as

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
              [(x, "")] -> Just x
              _ -> Nothing


firstRequest = "https://zkillboard.com/api/losses/factionID/500001/no-items/xml/"
nextRequest :: Int -> String
nextRequest kid =  "https://zkillboard.com/api/losses/afterKillID/"++ killStr ++"/factionID/500001/no-items/xml/"
    where killStr = show kid

zKillDiag :: Response b -> IO ()
zKillDiag r = do
    BS.putStrLn $ fromJust $ lookup "X-Bin-Request-Count" hs
    BS.putStrLn $ fromJust $ lookup "X-Bin-Max-Requests" hs
    where hs = responseHeaders r 

--responseToTraitors :: Response a -> Maybe (Int, [(Int, Toon)])
responseToTraitors res =
    case jks of
        Nothing -> Nothing
        Just kms -> return (
            foldl max 0 $ map killID $ kms
          , concat $ map getTraitors $ kms
          )
    where parsed = parseXMLDoc (responseBody res)
          jks = parsed >>= xmlemToKillmails

prettyTraitors :: [(Int, Toon)] -> IO ()
prettyTraitors =  flip forM_ prettyTraitor
    where
        prettyTraitor (i, t) = putStrLn $ urlPart i ++ factionPart t ++ namePart t
        urlPart id = "https://zkillboard.com/kill/" ++ show id ++ "/ "
        factionPart (Toon _ 500001 _) = "Caldari State's "
        factionPart (Toon _ 500003 _) = "Amarr Empire's  "
        factionPart _ = "Unknown?"
        namePart (Toon _ _ c) = c
        

zKillQuery :: Manager -> String -> IO Int
zKillQuery m s =
    case parseUrl s of
        Nothing -> print "Failure\n" >> exitFailure
        Just req -> do
            res <- httpLbs req m
            zKillDiag res
            let (kid, ts) = fromMaybe (0, []) $ responseToTraitors res
            prettyTraitors ts
            return kid

delayTime = 1000000 * 60* 5

nextQuery :: Manager -> StateT Int IO ()
nextQuery m = do
    liftIO $ threadDelay delayTime
    kid <- get
    nkid <- liftIO $ zKillQuery m $ nextRequest kid
    case nkid of
        0 -> put kid
        n -> put n


awaywoxMain :: IO ()
awaywoxMain = do
    m <- newManager conduitManagerSettings
    kid <- zKillQuery m firstRequest
    evalStateT (forever $ nextQuery m) kid

{-
            putStrLn $ take 1000 $ show $ responseBody res
            putStrLn $ show $ parseXMLDoc $ responseBody res
nextRequest :: Int -> Request_String
nextRequest kid =  getRequest $ "https://zkillboard.com/api/losses/afterKillID/"++ killStr ++"/factionID/500001/no-items/"
    where killStr = show kid
firstQuery :: IO Int
firstQuery = 

queryLoop :: StateT Int IO ()
queryLoop

main = forever $ do
    killID <- firstQuery
    runState forever queryLoop

main = interact $ (\x -> do
    show (parseXMLDoc x >>= xmlemToKillmails))
-}

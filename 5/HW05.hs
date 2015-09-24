{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module HW05 where

import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import System.Environment (getArgs)
import Data.Bits (xor)
import Data.List (sortBy)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map

import Parser

-- Exercise 1 -----------------------------------------

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret original message = do
  org <- BS.readFile original
  msg <- BS.readFile message
  return $ BS.pack $ filter (/= 0) $ zipWith xor (BS.unpack org) (BS.unpack msg)


-- Exercise 2 -----------------------------------------

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey key filepath = do
  encryptedFile <- BS.readFile (filepath ++ ".enc")
  let decrypted = BS.pack $ zipWith xor (BS.unpack encryptedFile) (BS.unpack $ BS.cycle key)
  BS.writeFile filepath decrypted
  

-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile json = do
  file <- BS.readFile json
  return $ decode file


-- Exercise 4 -----------------------------------------

filterTIds :: Maybe [TId] -> Maybe [Transaction] -> Maybe [Transaction]
filterTIds Nothing _ = Nothing
filterTIds _ Nothing = Nothing
filterTIds (Just tids) (Just transactions) = Just $ filter (\x -> (tid x) `elem` tids) transactions


getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs victims transactions = do
  victimsTIds <- parseFile victims :: IO (Maybe [TId])
  transactionsList <- parseFile transactions :: IO (Maybe [Transaction])
  return $ filterTIds victimsTIds transactionsList

-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow transactions = go transactions Map.empty
  where go :: [Transaction] -> Map String Integer -> Map String Integer
        go [] mapping = mapping
        go (x:xs) mapping = go xs (Map.unionWith (+) mapping (Map.fromList [(from x, -(amount x)),(to x, amount x)]))  

-- Exercise 6 -----------------------------------------

getMaxMapValue :: Map String Integer -> (String, Integer)
getMaxMapValue mapping = go (Map.toList mapping) ("", 0)
  where go :: [(String,Integer)] -> (String, Integer) -> (String, Integer)
        go [] acc = acc
        go (x:xs) acc 
          | snd x > snd acc = go xs x
          | otherwise = go xs acc
  

getCriminal :: Map String Integer -> String
getCriminal transactionFlows = fst $ getMaxMapValue transactionFlows

-- Exercise 7 -----------------------------------------

makeTransaction :: (String, Integer) -> (String, Integer) -> TId -> Transaction
makeTransaction (name1, amount1) (name2, amount2) tid = Transaction name1 name2 (abs (amount1 - amount2)) tid

makeTransactions :: [(String, Integer)] -> [(String, Integer)] -> [TId] -> [Transaction]
makeTransactions payers payees tids = go payers payees tids [] 
  where go :: [(String, Integer)] -> [(String, Integer)] -> [TId] -> [Transaction] -> [Transaction]
        go [] [] _ transactions = transactions
        go x y [] _ = error "Not enough new transaction codes supplied!"
        go x [] _ _ = error "Payers left, all payees refunded"
        go [] x _ _ = error "Payees left, all payers paid :S"
        go (x:xs) (y:ys) (tid:tids) transactions 
          -- The payer owes more than the payee is due 
          | snd x > abs (snd y) = go ((fst x, (snd x + snd y)):xs) ys tids ((makeTransaction x y tid) : transactions)
          -- The payers owes exactly what the payee is due 
          | snd x == abs (snd y) = go xs ys tids ((makeTransaction x y tid) : transactions) 
          -- The payer owes less than the payee is due
          | otherwise = go xs ((fst y, (snd y + snd x)):ys) tids ((makeTransaction x y tid) : transactions)
        
undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs mapping newTransactionCodes = makeTransactions sortedPayers sortedPayees newTransactionCodes
  where payers = filter (\(x,y) -> y > 0) $ Map.toList mapping
        payees = filter (\(x,y) -> y <= 0) $ Map.toList mapping 
        sortedPayers = reverse $ sortBy (\x y -> compare (snd x) (snd y)) payers
        sortedPayees = reverse $ sortBy (\x y -> compare (snd x) (snd y)) payees

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON out transaction = do
  BS.writeFile out $ encode $ toJSON transaction
  

-- Exercise 9 -----------------------------------------

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  decryptWithKey key vict
  mts <- getBadTs vict trans
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts       
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)

main :: IO ()
main = do
  args <- getArgs
  crim <- 
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "dog-original.jpg"
                        "dog.jpg"
                        "transactions.json"
                        "victims.json"
                        "new-ids.json"
                        "new-transactions.json"
  putStrLn crim


{-# LANGUAGE OverloadedStrings #-}
module PgnGame
    ( 
    pgnDatabaseParser
    , tagSection 
    , tagName
    , TagName(Fen)
    , tagValue
    , pgnBoard
    ) where
import Data.Word
import Data.Either
import Control.Lens
import qualified Data.Attoparsec.ByteString as BS 
import Data.Attoparsec.ByteString.Char8 hiding (take)
import Control.Applicative
-- show
import Data.ByteString.Char8 (ByteString,singleton)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.String
import Data.Char (toLower, isSpace)
import Data.Monoid hiding (Product)
import Data.Foldable (foldMap)

-- /show

-----------------------
------ SETTINGS -------
-----------------------

-----------------------
-------- TYPES --------
-----------------------
type PVMoves = [String]
parsePrincipalVariation :: Parser PVMoves 
parsePrincipalVariation = do
  manyTill anyChar (stringCI " pv ")
  pv <- many anyChar 
  return $ words pv
  
  
-- | Type game results
data GameResult = WhiteWins | BlackWins | DrawnGame | ResultUnknown deriving (Eq,Show,Enum)

-- | Parser of values of type 'GameResult'.
parseGameResult :: Parser GameResult
parseGameResult = 
     (string "1-0" >> return WhiteWins)
 <|> (string "0-1" >> return BlackWins)
 <|> (string "1/2-1/2" >> return DrawnGame)
 <|> (string "*" >> return ResultUnknown) 
  
-- | Type for tags

data TagName = Event | Site | Date | Round | White | Black | 
        Result | SetUp | Fen | OtherSupplemental String deriving (Eq, Show)

otherSupplParser :: Parser TagName
otherSupplParser = do
  v <- manyTill anyChar (char ' ')
  return $ OtherSupplemental v

tagNameParser :: Parser TagName
tagNameParser = 
     (stringCI "Event " >> return Event)
 <|> (stringCI "Site " >> return Site)
 <|> (stringCI "Date " >> return Date)
 <|> (stringCI "Round " >> return Round) 
 <|> (stringCI "Date " >> return Date)
 <|> (stringCI "White " >> return White) 
 <|> (stringCI "Black " >> return Black)
 <|> (stringCI "Result " >> return Result) 
 <|> (stringCI "SetUp " >> return SetUp)
 <|> (stringCI "Fen " >> return Fen) 
 <|> otherSupplParser

data TagPair = 
  TagPair { tagName :: TagName
          , tagValue :: String
            } deriving (Eq, Show)
          
 
tagValueParser :: Parser String 
tagValueParser = do
  manyTill anyChar (char '"')
  manyTill anyChar (char '"') <* skipSpace
  

tagPairParser :: Parser TagPair
tagPairParser = do
    char '['
    tn <- tagNameParser
    tv <- tagValueParser
    char ']'
    return $ TagPair tn tv
    
type TagSection = [TagPair]

tagSectionParser :: Parser TagSection
tagSectionParser = many $ tagPairParser <* endOfLine

type Move = ByteString

{-
movesParser :: Parser [Move]
movesParser = many $ moveParser

moveParser :: Parser Move
moveParser = do
   n <- parseMoveNumber
   m <- parseSANMove 
   a <- parseAnnoGlyph
-}

data MoveSection =
  MoveSection { moves :: [Move]
              , gameTermination :: GameResult
                } deriving (Eq, Show) 
                
moveSectionParser :: Parser MoveSection
moveSectionParser = do
    -- ms <- movesParser
    gt <- parseGameResult
    return $ MoveSection [] gt

data PgnGame =
  PgnGame  { tagSection :: TagSection
           , moveSection :: MoveSection
             } deriving (Eq, Show)

isEolOrBlank w = isEol w || w == 32
isEol w = w == 13 || w == 10

pgnGameParser :: Parser PgnGame
pgnGameParser = do
    ts <- tagSectionParser
    BS.skipWhile isEolOrBlank
    ms <- moveSectionParser
    BS.skipWhile isEolOrBlank
    return $ PgnGame ts ms 
             
type PgnDatabase = [PgnGame]

pgnDatabaseParser :: Parser PgnDatabase
pgnDatabaseParser = many $ pgnGameParser
-----------------------
------- PARSING -------
-----------------------


type Fen = ByteString
type FenEnhanced = ByteString

-- foldr:: (a->b->b)->b->[a]->b

--split :: String -> Char -> [String]
split s c = foldr (\x acc -> if x==c then [] : acc else (x : head acc) : tail acc) [[]] s
expand '1' = "."
expand '2' = ".."
expand '3' = "..."
expand '4' = "...."
expand '5' = "....."
expand '6' = "......"
expand '7' = "......."                   
expand '8' = "........"      
expand x = [x]


zippedLines = zip [0..7] ['a'..'h']
getLineByChar ch = [ n | (n,c) <- zippedLines, c == ch ] !! 0

zippedRows = zip [7,6..0] ['1'..'8']
getRowByChar ch = [ n | (n,c) <- zippedRows, c == ch ] !! 0

pgnBoard :: (Show a) => a -> String -> String -> ByteString -> String
pgnBoard no fen m pv =  let 
                           inx (a,b) = b * 9 + a
                           board = words fen !! 0
                           orient = " " ++ words fen !! 1 ++ " "
                           expandedBoard = concat [expand x | x <- board]
                           colRow s = let (f1,f2) = (s !! 0, s !! 1)
                                  in (getLineByChar f1,  getRowByChar f2)
                           frmIx = inx $ colRow $ take 2 m
                           toIx = inx $ colRow $ drop 2 m
                           movedPiece = '(' : expandedBoard !! frmIx : [')'] 
                           frmBoard = expandedBoard & ix frmIx .~ '+' 
                           newBoard b p ix = let splFen = splitAt ix b 
                                             in fst splFen ++ p ++ (tail $ snd splFen)
                           ob =  newBoard frmBoard "(+)" frmIx
                           nb = let toIx' = if toIx > frmIx then toIx + 2 else toIx
                                in newBoard ob movedPiece toIx' 
                           pv' =  parseOnly parsePrincipalVariation pv
                        in expandedBoard ++ orient ++ "\t\t" ++ 
                           nb ++ orient ++ 
                           "\tMove: " ++ (unwords . take 6 $ fromRight [] pv') ++ "\t" ++
                           fen ++ "\t" ++ show no -- m



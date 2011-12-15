{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
-- | Copyright: Simon Meier <iridcode@gmail.com>
--
-- Baseline estimate of serialization speed.
module Main where

import           Criterion.Main (defaultMain, bench, whnf)

import           Data.Bits
import           Data.ByteString.Lazy.Builder
import           Data.ByteString.Lazy.Builder.Extras
import           Data.ByteString.Lazy.Builder.BasicEncoding 
                 ( fromF, pairB, ifB, (>$<) )
import qualified Data.ByteString.Lazy.Builder.BasicEncoding        as E
import qualified Data.ByteString.Lazy.Builder.BasicEncoding.Extras as E
import qualified Data.ByteString                                   as S
import qualified Data.ByteString.Lazy                              as L
import           Data.Foldable (foldMap)
import qualified Data.IntMap                                       as IM
import           Data.Monoid (Monoid(..), mappend)

import Foreign


{-
message Image {
  required string uri = 1;      //url to the thumbnail
  optional string title = 2;    //used in the html ALT
  required int32 width = 3;     // of the image
  required int32 height = 4;    // of the image
  enum Size {
    SMALL = 0;
    LARGE = 1;
  }
  required Size size = 5;       // of the image (in relative terms, provided by cnbc for example)
}

message Media {
  required string uri = 1;      //uri to the video, may not be an actual URL
  optional string title = 2;    //used in the html ALT
  required int32 width = 3;     // of the video
  required int32 height = 4;    // of the video
  required string format = 5;   //avi, jpg, youtube, cnbc, audio/mpeg formats ...
  required int64 duration = 6;  //time in miliseconds
  required int64 size = 7;      //file size
  optional int32 bitrate = 8;   //video 
  repeated string person = 9;   //name of a person featured in the video
  enum Player {
    JAVA = 0;
    FLASH = 1;
  }
  required Player player = 10;   //in case of a player specific media
  optional string copyright = 11;//media copyright
}

message MediaContent {
  repeated Image image = 1;
  required Media media = 2;
}
-}

data Size = SMALL | LARGE
       deriving( Enum )

data Image = Image 
       String                    -- 1: url to the thumbnail
       (Maybe String)            -- 2: url in the html ALT
       {-# UNPACK #-} !Int32     -- 3: width of the image
       {-# UNPACK #-} !Int32     -- 4: height of the image
       Size                      -- 5: size of the image (in relative terms)

data Player = JAVA | FLASH
        deriving( Enum )
       
data Media = Media
       String                  -- 1: uri to the video, may not be an actual URL
       (Maybe String)          -- 2: title used in the html ALT
       {-# UNPACK #-} !Int32   -- 3: width of the video
       {-# UNPACK #-} !Int32   -- 4: height of the video
       String                  -- 5: format: avi, jpg, youtube, cnbc, audio/mpeg formats ...
       {-# UNPACK #-} !Int64   -- 6: duration time in miliseconds
       {-# UNPACK #-} !Int64   -- 7: file size
       (Maybe Int32)           -- 8: bitrate of the video 
       [String]                -- 9: names of people featured in the video
       Player                  -- 10: in case of a player specific media
       (Maybe String)          -- 11: media copyright

data MediaContent = MediaContent
       [Image]        -- 1: images
       Media          -- 2: media

testImage :: Image
testImage =
    Image
      "http://javaone.com/keynote_large.jpg"
      (Just "Javaone Keynote")
      1024
      768
      LARGE

-- |The standard test value.
{-# NOINLINE testValue #-}
testValue :: MediaContent
testValue = MediaContent
    [ Image
        "http://javaone.com/keynote_large.jpg"
        (Just "Javaone Keynote")
        1024
        768
        LARGE
      
    , Image
        "http://javaone.com/keynote_small.jpg"
        (Just "Javaone Keynote")
        320
        240
        SMALL
    ]
    ( Media
        "http://javaone.com/keynote.mpg"
        (Just "Javaone Keynote")
        640
        480
        "video/mpg4"
        18000000        -- half an hour in milliseconds
        58982400        -- bitrate * duration in seconds / 8 bits per byte
        (Just 262144)   -- 256k
        ["Bill Gates", "Steve Jobs"]
        JAVA
        Nothing
    )

------------------------------------------------------------------------------
-- Binary encodings
------------------------------------------------------------------------------

-- | Abbreviate 'mappend'.
(<>) :: Monoid m => m -> m -> m
(<>) = mappend

-- Manual encoding in custom binary format (similar to the 'binary' library)
----------------------------------------------------------------------------

binString :: String -> Builder
binString cs = 
    E.encodeListWithB taggedCharUtf8 cs <> word8 0
  where
    taggedCharUtf8 = (\c -> (1, c)) >$< fromF E.word8 `pairB` E.charUtf8

binSize :: Size -> Builder
binSize = word8 . fromIntegral . fromEnum

binMaybe :: (a -> Builder) -> Maybe a -> Builder
binMaybe _ Nothing  = word8 0
binMaybe f (Just x) = word8 1 <> f x

binMayString :: Maybe String -> Builder
binMayString = binMaybe binString

binList :: (a -> Builder) -> [a] -> Builder
binList f xs = foldMap (\x -> word8 1 <> f x) xs <> word8 0

binImage :: Image -> Builder
binImage (Image url title w h s) = 
  binString url <> binMayString title <> int32LE w <> int32LE h <> binSize s

binPlayer :: Player -> Builder
binPlayer = word8 . fromIntegral . fromEnum

binMedia :: Media -> Builder
binMedia (Media uri title w h format duration s rate people player copy) =
  binString uri <> binMayString title <> int32LE w <> int32LE h <>
  binString format <> int64LE duration <> int64LE s <> 
  binMaybe int32LE rate <> binList binString people <> binPlayer player <>
  binMayString copy

binMediaContent :: MediaContent -> Builder
binMediaContent (MediaContent images media) = 
  binList binImage images <> binMedia media

-- Google's protocol buffer format
----------------------------------

-- To be done.

-- | We use tags extended with field type and packed, signed, info to index
-- the fields.
newtype Msg = Msg { getMsg :: IM.IntMap Field }
          deriving( Eq, Ord, Show )

data Field = 
         Int32    {-# UNPACK #-} !Int32   -- ^ variable length int32, enum, bool
                                          -- ^ uint32
       | Int64    {-# UNPACK #-} !Int64   -- ^ variable length int64, uint64
       -- | SInt32   {-# UNPACK #-} !Int32   -- ^ zig-zag, variable length int32
       -- | SInt64   {-# UNPACK #-} !Int64   -- ^ zig-zag, variable length int64
       -- | FInt32   {-# UNPACK #-} !Int32   -- ^ fixed-size int32
       -- | FInt64   {-# UNPACK #-} !Int64   -- ^ fixed-size int64
       -- | Double   {-# UNPACK #-} !Double  -- ^ A 'Double' value.
       | String   String                  -- TODO: Use 'text' package
       -- | Bytes    S.ByteString
       | Message  Msg
       -- TODO: use vector package for repeated fields.
       -- | Int32V   [Int32]                 -- ^ A repeated primitive field.
       -- | Int64V   [Int64]  
       -- | SInt32V  [Int32]
       -- | SInt64V  [Int64]
       -- | FInt32V  [Int32]
       -- | FInt64V  [Int64]
       -- | DoubleV  [Double]
       -- | BoolV    [Bool]
       | StringV  [String]
       | MessageV [Msg]
         -- ^ Embedded message TODO: allow caching via bytestring
       deriving( Eq, Ord, Show )

-- packed, zigZagged, fixed

-- | 'FieldId's are the field numbers used in the .proto specification.
type FieldId = Int32

-- | 'Tag's are the field numbers together with their wiretype.
type Tag = Int

encodeWithVarLen :: Builder -> Builder
encodeWithVarLen = 
    E.encodeWithSize (fromIntegral defaultChunkSize) E.word64VarFixedBound

renderField :: Tag -> Field -> Builder
renderField tag (Int32 i) = 
    E.encodeWithB encInt32 (fromIntegral tag, i)
  where
    encInt32 = E.int32Var `pairB` E.int32Var
renderField tag (Int64 i) =
    E.encodeWithB encInt64 (fromIntegral tag, i)
  where
    encInt64 = E.int32Var `pairB` E.int64Var
renderField tag (String cs)     = renderTaggedString  tag cs
renderField tag (Message msg)   = renderTaggedMessage tag msg
renderField tag (StringV css)   = foldMap (renderTaggedString  tag) css
renderField tag (MessageV msgs) = foldMap (renderTaggedMessage tag) msgs

renderTaggedString :: Tag -> String -> Builder
renderTaggedString tag cs = 
    E.encodeWithB E.int32Var (fromIntegral tag) <>
    encodeWithVarLen (stringUtf8 cs)

renderTaggedMessage :: Tag -> Msg -> Builder
renderTaggedMessage tag msg = 
    E.encodeWithB E.int32Var (fromIntegral tag) <>
    encodeWithVarLen (renderMessage msg)

renderMessage :: Msg -> Builder
renderMessage = foldMap (uncurry renderField) . IM.toAscList . getMsg

-- Internal message construction functions.
-------------------------------------------

-- | Add an optional field to a field map. If it is 'Nothing', then the map
-- remains unchanged.
optional :: (a -> (Int, b)) -> Maybe a -> IM.IntMap b -> IM.IntMap b
optional _ Nothing  = id
optional f (Just x) = uncurry IM.insert (f x)

-- | Add a repeated field to an field map. If it is 'Empty', then the map
-- remains unchanged.
repeated :: ([a] -> (Int, b)) -> [a] -> IM.IntMap b -> IM.IntMap b
repeated _ [] = id
repeated f xs = uncurry IM.insert (f xs)

-- | Tag a value together with its field and wire type.
asWireType :: ToField a => Int -> a -> FieldId -> (Tag, Field)
asWireType ty x fid = ((fromIntegral fid `shiftL` 3) .|. ty, toField x)

-- | Tag a value with its field id as a variable length wire type.
asVar :: ToField a => a -> FieldId -> (Tag, Field)
asVar = asWireType 0

-- | Tag a value with its field id as a length-delimited wire type.
asLDelim :: ToField a => a -> FieldId -> (Tag, Field)
asLDelim = asWireType 2


-- | A class that abstracts the conversion of Haskell values to the
-- corresponding unityped 'Field' value.
class ToField a where
    toField :: a -> Field

instance ToField Int32 where
    toField = Int32

instance ToField Int64 where
    toField = Int64

instance ToField String where
    toField = String

instance ToField [String] where
    toField = StringV

instance ToField Msg where
    toField = Message

instance ToField [Msg] where
    toField = MessageV


-- Conversion of custom types
-----------------------------

instance ToField Size where
    toField = Int32 . fromIntegral . fromEnum

instance ToField Player where
    toField = Int32 . fromIntegral . fromEnum

instance ToField Image where
    toField = Message . imageMsg

instance ToField Media where
    toField (Media uri title width height format duration size rate 
                   people player copy) = 
      Message $ Msg $ 
        optional (`asLDelim`  2) title $
        repeated (`asLDelim`  9) people $
        optional (`asVar`    8)  rate $
        optional (`asLDelim` 11) copy $
        IM.fromList
          [ uri      `asLDelim` 1
          , width    `asVar`    3
          , height   `asVar`    4
          , format   `asLDelim` 5
          , duration `asVar`    6
          , size     `asVar`    7
          , player   `asVar`   10
          ]

instance ToField [Image] where
    toField = MessageV . map imageMsg

instance ToField MediaContent where
    toField = Message . mediaContentMsg

mediaContentMsg :: MediaContent -> Msg
mediaContentMsg (MediaContent images media) = Msg $ 
    repeated (`asLDelim` 1) images $
    IM.fromList
      [ media  `asLDelim` 2 ]

imageMsg :: Image -> Msg
imageMsg (Image uri title width height size) = Msg $ 
    optional (`asLDelim` 2) title $
    IM.fromList
      [ uri    `asLDelim` 1
      , width  `asVar`    3
      , height `asVar`    4
      , size   `asVar`    5
      ]


-- A draft of the actual interface
----------------------------------

-- One type per message type.
-- One fclabels accessor per required field.
-- x = Image.title becomes
-- get imageTitle img




------------------------------------------------------------------------------
-- Benchmarking code
------------------------------------------------------------------------------

testValues = replicate 100 testValue

main :: IO ()
main = defaultMain
  [ bench "100 protobuf/generic" $ whnf 
        (L.length . toLazyByteString . foldMap (renderMessage . mediaContentMsg)) testValues
  , bench "100 bin/manual" $ whnf 
        (L.length . toLazyByteString . foldMap (binMediaContent)) testValues
  , bench "protobuf/generic" $ whnf 
        (L.length . toLazyByteString . renderMessage . mediaContentMsg) testValue
  , bench "bin/manual" $ whnf 
        (L.length . toLazyByteString . binMediaContent) testValue
  ]




{- BIT PACKING EXPERIMENT


-- | 'FieldId's are tags shifted left by 2 bits. Bit 0 states whether a fixed
-- size encoding should be used, Bit 1 states whether a zig-zagged encoding
-- should be used.
type FieldId = Int

isFixed :: Int -> Bool
isFixed = (`testBit` 0)

isZigZagged :: Int -> Bool
isZigZagged = (`testBit` 1)

isPacked :: Int -> Bool
isPacked = (`testBit` 2)


-- int32Tag :: Tag -> Int32 -> (Int, Field)
-- int32 tag i = (tag

-- repeatedInt32 :: Tag -> [Int32] -> (FieldId, Field)
-- repeatedInt32 t is = (

-- packedInt32 ::

asType :: FieldId -> Int -> Tag
asType fid ty = fromIntegral ((fid .&. 0xfff8) .|. ty)

fromTag :: Tag -> FieldId
fromTag t = fromIntegral (t `shiftL` 3)

-}

{- Required for parsing
instance Monoid Msg where
    mempty = Msg IM.empty
    m1 `mappend` m2 = Msg $ IM.unionWith mergeFields (getMsg m1) (getMsg m2)

mergeFields :: Field -> Field -> Field
mergeFields = m
  where
    m (String x) (String y) = String (x ++ y)
    m _          _          = error "mergeFields: implement"
-}


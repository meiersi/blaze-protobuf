-- | Copyright: Simon Meier <iridcode@gmail.com>
--
-- Baseline estimate of serialization speed.
module Main where

import           Criterion.Main (defaultMain, bench, whnf)

import           Data.Monoid (Monoid(..), mappend)
import           Data.Foldable (foldMap)
import           Data.ByteString.Lazy.Builder
import           Data.ByteString.Lazy.Builder.BasicEncoding 
                 ( fromF, pairB, ifB, (>$<) )
import qualified Data.ByteString.Lazy.Builder.BasicEncoding as E
import qualified Data.ByteString.Lazy                       as L

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

------------------------------------------------------------------------------
-- Benchmarking code
------------------------------------------------------------------------------

main :: IO ()
main = defaultMain
  [ bench "bin/manual" $ whnf 
        (L.length . toLazyByteString . binMediaContent) testValue
  ]

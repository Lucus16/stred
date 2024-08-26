module Stred.Image
  ( Sized (..)
  , Image
  , raw
  , ishow
  , crop
  , hcat
  , vcat
  , renderImage
  , bold
  , fg
  , bg
  , underline
  ) where

import Control.Applicative ((<|>))
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word8)
import Graphics.Vty qualified as Vty

type Color = Word8

data Image
  = Raw Text
  | Styled Style (Sized Image)
  | HCat (NonEmpty (Sized Image))
  | VCat (NonEmpty (Sized Image))

raw :: Text -> Sized Image
raw t = Sized (Vty.wctwidth t) 1 (Raw t)

ishow :: (Show a) => a -> Sized Image
ishow = raw . Text.pack . show

crop :: Int -> Int -> Sized Image -> Sized Image
crop w h (Sized _ _ img) = Sized w h img

style :: Style -> Sized Image -> Sized Image
style s (Sized w h (Styled s' image)) = Sized w h (Styled (s `over` s') image)
style s image = Sized w h (Styled s image)
  where
    Sized w h _ = image

hcat :: NonEmpty (Sized Image) -> Sized Image
hcat (img :| []) = img
hcat imgs = Sized (sum widths) (maximum heights) (HCat imgs)
  where
    widths = width <$> imgs
    heights = height <$> imgs

vcat :: NonEmpty (Sized Image) -> Sized Image
vcat (img :| []) = img
vcat imgs = Sized (maximum widths) (sum heights) (VCat imgs)
  where
    widths = width <$> imgs
    heights = height <$> imgs

data Sized a = Sized
  { width :: Int
  , height :: Int
  , unSized :: a
  }

instance IsString (Sized Image) where
  fromString = raw . Text.pack

renderLine :: Style -> Int -> Sized Image -> StyledLine
renderLine outerStyle line (Sized w h image)
  | line >= h = styleLine outerStyle "" w
  | otherwise = case image of
      Raw t -> styleLine outerStyle t w
      Styled innerStyle img -> renderLine (innerStyle `over` outerStyle) line img{width = w}
      HCat (img0 :| imgs) -> case nonEmpty imgs of
        Just imgs'
          | w > width img0 && w > width img0 ->
              renderLine outerStyle line img0
                <> renderLine outerStyle line (Sized (w - width img0) h (HCat imgs'))
        _ -> renderLine outerStyle line img0{width = w}
      VCat (img0 :| imgs)
        | line < height img0 -> renderLine outerStyle line img0{width = w}
        | otherwise -> case nonEmpty imgs of
            Just imgs'
              | h > height img0 ->
                  renderLine outerStyle (line - height img0) $ Sized w (h - height img0) (VCat imgs')
            _ -> styleLine outerStyle "" w

renderImage :: Sized Image -> ByteString
renderImage image = clearScreen <> mconcat (doLine <$> [0 .. height image - 1])
  where
    doLine lineno =
      styleTransition defaultStyle startStyle
        <> unStyled
        <> styleTransition endStyle defaultStyle
        <> "\n"
      where
        StyledLine{startStyle, endStyle, unStyled} = renderLine defaultStyle lineno image

data StyledLine = StyledLine
  { startStyle :: Style
  , endStyle :: Style
  , trailingSpace :: Int
  , unStyled :: ByteString
  }

instance Semigroup StyledLine where
  x <> y =
    StyledLine
      { startStyle = startStyle x
      , endStyle = endStyle y
      , trailingSpace = trailingSpace y
      , unStyled =
          unStyled x
            <> Data.ByteString.Char8.replicate (trailingSpace x) ' '
            <> styleTransition (endStyle x) (startStyle y)
            <> unStyled y
      }

styleLine :: Style -> Text -> Int -> StyledLine
styleLine s text targetWidth =
  StyledLine
    { startStyle = s
    , endStyle = s
    , trailingSpace = targetWidth - Vty.wctwidth cropped
    , unStyled = encodeUtf8 cropped
    }
  where
    cropped = cropLine text

    cropLine :: Text -> Text
    cropLine t
      | Vty.wctwidth t <= targetWidth = t
      | otherwise = case Text.unsnoc t of
          Nothing -> ""
          Just (t', _) -> cropLine t'

bold :: Sized Image -> Sized Image
bold = style defaultStyle{sBold = Just True}

fg :: Color -> Sized Image -> Sized Image
fg c = style defaultStyle{fgColor = Just c}

bg :: Color -> Sized Image -> Sized Image
bg c = style defaultStyle{bgColor = Just c}

underline :: Color -> Sized Image -> Sized Image
underline c = style defaultStyle{ulColor = Just c}

data Style = Style
  { sBold :: Maybe Bool
  , bgColor :: Maybe Color
  , fgColor :: Maybe Color
  , ulColor :: Maybe Color
  }
  deriving (Eq)

defaultStyle :: Style
defaultStyle =
  Style
    { sBold = Nothing
    , bgColor = Nothing
    , fgColor = Nothing
    , ulColor = Nothing
    }

over :: Style -> Style -> Style
over x y =
  Style
    { sBold = sBold x <|> sBold y
    , bgColor = bgColor x <|> bgColor y
    , fgColor = fgColor x <|> fgColor y
    , ulColor = ulColor x <|> ulColor y
    }

styleTransition :: Style -> Style -> ByteString
styleTransition before after
  | before /= defaultStyle && after == defaultStyle = sgr []
  | otherwise =
      mconcat
        [ handleField sBold \case
            Just True -> sgr [1]
            _ -> sgr [22]
        , handleField fgColor \case
            Nothing -> sgr [39]
            Just color -> sgr [38, 5, fromIntegral color]
        , handleField bgColor \case
            Nothing -> sgr [49]
            Just color -> sgr [48, 5, fromIntegral color]
        , handleField ulColor \case
            -- Nothing -> sgr [24, 59]
            -- Just color -> sgr [4, 58, 5, fromIntegral color]
            Nothing -> sgr [24]
            Just _ -> sgr [4]
        ]
  where
    handleField :: (Eq a) => (Style -> Maybe a) -> (Maybe a -> ByteString) -> ByteString
    handleField field f
      | field before == field after = ""
      | otherwise = f (field after)

    sgr :: [Int] -> ByteString
    sgr xs = csi $ ByteString.intercalate ";" (bshow <$> xs) <> "m"

csi :: ByteString -> ByteString
csi x = ByteString.singleton 27 <> "[" <> x

clearScreen :: ByteString
clearScreen = csi "2J" <> csi "H" <> csi "?25l"

bshow :: (Show a) => a -> ByteString
bshow = encodeUtf8 . Text.pack . show

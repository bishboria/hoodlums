import Prelude hiding ((.), id)

import Control.Wire
import Control.Arrow
import Data.Set (Set)
import Data.Monoid
import qualified Data.Set as S
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as SDLi

type Point = (Double, Double)

imagePath = "./mario.png"

main :: IO ()
main = SDL.withInit [SDL.InitEverything] $ do
  screen <- SDL.setVideoMode 640 480 32 [SDL.SWSurface]
  SDL.setCaption "Netwire Test" "netwire test"
  tile <- SDLi.load imagePath
  go screen tile clockSession position (S.empty, 0)

 where

  go screen tile s w (keyPresses, y) = do
    ((x', y'), w', s') <- stepSession_ w s (keyPresses, y)

    (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 150 150 255 >>=
        SDL.fillRect screen Nothing

    (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 50 50 0 >>=
        SDL.fillRect screen (Just $ SDL.Rect 0 383 640 97)

    let r = Just (SDL.Rect (round x') (round y') 640 480)
    SDL.blitSurface tile r screen Nothing

    SDL.flip screen

    keyPresses' <- parseEvents keyPresses
    go screen tile s' w' (keyPresses', y')

position :: (Monoid e, Monad m) => Wire e m (Set SDL.Keysym, Double) Point
position = integral_ (0, 0) . velocity

velocity :: (Monoid e, Monad m) => Wire e m (Set SDL.Keysym, Double) Point
velocity = velocityX *** velocityY


velocityY :: (Monoid e, Monad m) => Wire e m Double Double
velocityY  = integral_ 0 . (pure (-9.81)) . when (> -300)
         <|> pure 0

velocityX :: (Monoid e, Monad m) => Wire e m (Set SDL.Keysym) Double
velocityX  =  pure (500) . when (keyDown SDL.SDLK_LEFT)
          <|> pure (-500) . when (keyDown SDL.SDLK_RIGHT)
          <|> pure (0)

parseEvents :: Set SDL.Keysym -> IO (Set SDL.Keysym)
parseEvents keysDown = do
  event <- SDL.pollEvent
  case event of
    SDL.NoEvent -> return keysDown
    SDL.KeyDown k -> parseEvents (S.insert k keysDown)
    SDL.KeyUp k -> parseEvents (S.delete k keysDown)
    _ -> parseEvents keysDown

keyDown :: SDL.SDLKey -> Set SDL.Keysym -> Bool
keyDown k = not . S.null . S.filter ((== k) . SDL.symKey)

instance Ord (SDL.Keysym) where
  k `compare` k' = SDL.symUnicode k `compare` SDL.symUnicode k'


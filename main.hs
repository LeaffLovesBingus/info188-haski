import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy (loadJuicyPNG)


window :: Display
window = InWindow "Haski" (1280, 720) (100, 100)

background :: Color
background = black

fps :: Int
fps = 60

data World = World
    { picture :: Picture  
    }


main :: IO ()
main = do
    maybeImg <- loadJuicyPNG "assets/ojohohojojohohjoohohhojoohojoh.png"
    let img = case maybeImg of
                Just p  -> p
                Nothing -> Color red (Text "Error vro")
    play
        window
        background
        fps
        (World img)
        render
        handleEvent
        update

render :: World -> Picture
render w = Translate 0 0 $ Scale 0.5 0.5 (picture w)

handleEvent :: Event -> World -> World
handleEvent _ w = w

update :: Float -> World -> World
update _ w = w
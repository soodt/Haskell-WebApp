{-# LANGUAGE OverloadedStrings #-}

module Main where
import Web.Scotty
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.ByteString.Lazy (ByteString)
import Codec.Picture
import DrawingDSL ( renderDrawing, Drawing, exampleAllShapes, exampleAllTransformations, exampleAllCompositions)
import Control.Monad.IO.Class
import Text.Blaze.Html.Renderer.Text (renderHtml)

saveDrawingToFile :: FilePath -> Drawing -> IO ByteString
saveDrawingToFile filename drawing = do
    img <- renderDrawing drawing
    writePng filename img
    return $ encodePng img

main :: IO ()
main = scotty 3000 $ do
    -- Home route with links to each example
    get "/" $ do
        html $ renderHtml $ H.docTypeHtml $ do
            H.head $ H.title "Drawing DSL Examples"
            H.body $ do
                H.h1 "Drawing DSL Examples"
                H.p $ H.a H.! A.href "/shapes" $ "View Shapes Example"
                H.p $ H.a H.! A.href "/transformations" $ "View Transformations Example"
                H.p $ H.a H.! A.href "/compositions" $ "View Compositions Example"
    -- Route to view example with all shapes
    get "/shapes" $ do
        let drawing = exampleAllShapes
        _ <- liftIO $ saveDrawingToFile "./example_shapes.png" drawing
        html $ renderHtml $ H.docTypeHtml $ do
            H.head $ H.title "Shapes Example"
            H.body $ do
                H.h1 "Shapes Example"
                H.img H.! A.src "/shapes_image"
                H.h2 "DSL Code"
                H.pre $ H.toHtml (show drawing)
    -- Serve the shapes example image
    get "/shapes_image" $ do
        pngImage <- liftIO $ saveDrawingToFile "./example_shapes.png" exampleAllShapes
        setHeader "Content-Type" "image/png"
        raw pngImage
    -- Route to view example with all transformations
    get "/transformations" $ do
        let drawing = exampleAllTransformations
        _ <- liftIO $ saveDrawingToFile "./example_transformations.png" drawing
        html $ renderHtml $ H.docTypeHtml $ do
            H.head $ H.title "Transformations Example"
            H.body $ do
                H.h1 "Transformations Example"
                H.img H.! A.src "/transformations_image"
                H.h2 "DSL Code"
                H.pre $ H.toHtml (show drawing)
    -- Serve the transformations example image
    get "/transformations_image" $ do
        pngImage <- liftIO $ saveDrawingToFile "./example_transformations.png" exampleAllTransformations
        setHeader "Content-Type" "image/png"
        raw pngImage
    -- Route to view example with all compositions
    get "/compositions" $ do
        let drawing = exampleAllCompositions
        _ <- liftIO $ saveDrawingToFile "./example_compositions.png" drawing
        html $ renderHtml $ H.docTypeHtml $ do
            H.head $ H.title "Compositions Example"
            H.body $ do
                H.h1 "Compositions Example"
                H.img H.! A.src "/compositions_image"
                H.h2 "DSL Code"
                H.pre $ H.toHtml (show drawing)
    -- Serve the compositions example image
    get "/compositions_image" $ do
        pngImage <- liftIO $ saveDrawingToFile "./example_compositions.png" exampleAllCompositions
        setHeader "Content-Type" "image/png"
        raw pngImage
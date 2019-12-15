{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Wai.Middleware.Static
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forM_)
import Web.Spock
import Web.Spock.Config
import Web.Spock.Lucid (lucid)
import Lucid

import System.IO.Unsafe
import System.Random
import Data.UUID
import Data.UUID.V4
import Data.Semigroup ((<>))
import Data.IORef
import Data.Text (Text)

-- data MySession = EmptySession
-- data MyAppState = DummyAppState (IORef Int)

-- data Comment = Comment { author :: Text, message :: Text }
data Book = Book { bookId :: Integer, author :: Text, title :: Text }

newtype ServerState = ServerState { books :: IORef [Book] }
type Server serv = SpockM () () ServerState serv

newUUID :: IO UUID
newUUID = randomIO


app :: Server ()
app = do 
        middleware $ staticPolicy ( addBase "static" )
        get root $ do
            books' <- getState >>= (liftIO . readIORef . books)
            lucid $ do
                head_ $ link_ [ rel_ "stylesheet"
                                , type_ "text/css"
                                , href_ "/main.css"
                                ]
                h1_ "Hello!"
                ul_ $ forM_ books' $ \book -> li_ $ do
                    toHtml (show (bookId book))
                    ":"
                    toHtml (author book)
                    ":"
                    toHtml (title book)
                    a_ [href_ "/", class_ "delete-button"] "" :: Html()
                p_ "Helloooooooooooo!"
                a_ [ href_ "addBook"] "Add Books" :: Html ()
        get ("addBook") $ lucid $ do
                h2_ "New Book"
                form_ [ method_ "post" ] $ do
                    label_ $ do 
                        "Author: "
                        input_ [name_ "author"]
                    label_ $ do 
                        "Title: "
                        input_ [name_ "title"]
                    input_ [ type_ "submit", value_ "Add Book"] 
        post ("addBook") $ do
            author <- param' "author"
            title <- param' "title"
            booksRef <- books <$> getState
            liftIO $ atomicModifyIORef' booksRef $ \books ->
                (books <> [Book ((bookId (last books)) + 1) author title], ())
            redirect "/"

main :: IO ()
main = do 
    state <- ServerState <$> newIORef [
        Book 1 "Author_1" "Title_1",
        Book 2 "Author_2" "Title_2"
        ]
    cfg <- defaultSpockCfg () PCNoDatabase state
    runSpock 8080 (spock cfg app)
    -- do ref <- newIORef 0
        -- spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (DummyAppState ref)
    -- app = 
--     do get root $
--         text "Hello World!"
--         get ("hello"<//> var) $ \name ->
--             do (DummyAppState ref) <- getState
--                 visitorNUmber <- liftIO $ atomicModifyIORef' ref $ \i -> (i+1, i+1)
--                 text("Hello " <> name <> ", you are visitor number " <> T.pack (show visitorNumber))

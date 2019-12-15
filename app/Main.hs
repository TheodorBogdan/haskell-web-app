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
import Data.Text (Text, pack, append)

data Book = Book { bookId :: Integer, author :: Text, title :: Text }

newtype ServerState = ServerState { books :: IORef [Book] }
type Server serv = SpockM () () ServerState serv



findBookById :: Integer -> [Book] -> Book
findBookById i (x: xs)
    | (bookId x) == i  = x
    | otherwise = findBookById i xs

removeItem :: Integer -> [Book] -> [Book]
removeItem _ []                 = []
removeItem id (y:ys) | id == (bookId y)    = removeItem id ys
                     | otherwise = y : removeItem id ys

-- updateBook :: Integer -> Book -> [Book] -> [Book]
-- updateBook bookIdParam myBook books = 
--     let update p | bookId p == bookIdParam = p { title = title myBook, author = author myBook }
--                  | otherwise = p
--     in map update books
 
update ::  Integer -> Book -> [Book] -> [Book]
update bookIdParam myBook books = 
    case books of
       []     -> []
       (p:ps) | bookId p == bookIdParam ->
                     p { title = title myBook, author = author myBook } : ps
              | otherwise ->
                     p : update bookIdParam myBook ps

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
                h1_ "Books list"
                ul_ $ forM_ books' $ \book -> li_ $ do
                    toHtml (show (bookId book))
                    ":"
                    toHtml (author book)
                    ":"
                    toHtml (title book)
                    a_ [href_ ( append "delete/" ( pack (show (bookId book)))), class_ "button delete-button"] "" :: Html()
                    a_ [href_ ( append "editBook/" ( pack (show (bookId book)))), class_ "button"] "+" :: Html()
                a_ [ href_ "addBook"] "Add Books" :: Html ()
        get ("editBook" <//> var) $ \varId -> do
            books' <- getState >>= (liftIO . readIORef . books)
            let varId' = read varId :: Integer
            let theBook = findBookById varId' books'
            lucid $ do
                h1_ ( toHtml (title theBook))
                form_ [ method_ "post" ] $ do
                    input_ [ name_ "bookId", value_ (pack (show (bookId theBook))), style_ "opacity:0" ]
                    label_ $ do 
                        "Author: "
                        input_ [name_ "author", value_ (author theBook)]
                    label_ $ do 
                        "Title: "
                        input_ [name_ "title", value_ (title theBook)]
                    input_ [ type_ "submit", value_ "Confirm"] 
                a_ [ href_ "/"] "Books list" :: Html ()

        post ("editBook" <//> var) $ \varId -> do
            let varId' = read varId :: Integer
            books' <- getState >>= (liftIO . readIORef . books)
            author <- param' "author"
            title <- param' "title"
            booksRef <- books <$> getState
            let book = Book varId' author title

            liftIO $ atomicModifyIORef' booksRef $ \books ->
                (update varId' book books', ())
            redirect "/"
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

        get ("delete" <//> var) $ \varId -> do
            let varId' = read varId :: Integer
            books' <- getState >>= (liftIO . readIORef . books)
            let theBook = findBookById varId' books'
            booksRef <- books <$> getState
            liftIO $ atomicModifyIORef' booksRef $ \books ->
                (removeItem varId' books', ())
            redirect "/"    
        get ("error") $ lucid $ do 
            h1_ ("Error")        


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

{- | Executable  : sakd
     Description : deamon for certain knives
     Copyright   : (c) 2025 Fred Mitchell & Atomlogik
     License     : MIT
     Maintainer  : fred.mitchell@atomlogik.de
     Stability   : stable
     Portability : portable
-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import DBus
import DBus.Client

-- Define the Ping method
ping :: MethodCall -> IO Reply
ping _ = return (ReplyReturn [])

-- Define the Hello method
hello :: String -> IO String
hello name = return ("Hello " ++ name ++ "!")

-- Convert hello to a DBus method
helloMethod :: MethodCall -> IO Reply
helloMethod msg = do
    let Just name = fromVariant (methodCallBody msg !! 0)
    response <- hello name
    return (ReplyReturn [toVariant response])

-- Define the interface
myInterface :: Interface
myInterface = defaultInterface
    { interfaceName = "com.example.HelloWorld"
    , interfaceMethods =
        [ makeMethod "com.example.HelloWorld" "Ping" ping
        , makeMethod "com.example.HelloWorld" "Hello" helloMethod
        ]
    }

-- Export the interface at the given object path
main :: IO ()
main = do
    client <- connectSession
    -- Request a name on the bus
    rnr <- requestName client "com.example.HelloWorld" []
    print rnr

    -- Export the interface
    export client "/hello_world" myInterface

    -- Keep the service running
    _ <- getLine
    return ()

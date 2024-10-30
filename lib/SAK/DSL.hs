{-# LANGUAGE GADTs #-}

module SAK.DSL where

data Header = Header String [Details] deriving Show
data Details = Description String
             | Author String String
             | Copyright String String
             deriving Show


header :: String -> [Details] -> Header
header name details = Header name Details

description :: String -> Details
description desc = Description desc

author :: String -> String -> Details
author name email = Author name email

copyright :: String -> String -> Details
copyright holder year = Copyright 











module Data.Binary.Serialise.CBOR.FlatTerm where

import {-# SOURCE #-} Data.Binary.Serialise.CBOR.Encoding

type FlatTerm = [TermToken]

data TermToken
instance Show TermToken

toFlatTerm :: Encoding -> FlatTerm
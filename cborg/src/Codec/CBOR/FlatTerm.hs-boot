module Codec.CBOR.FlatTerm where

import {-# SOURCE #-} Codec.CBOR.Encoding

type FlatTerm = [TermToken]

data TermToken
instance Show TermToken

toFlatTerm :: Encoding -> FlatTerm
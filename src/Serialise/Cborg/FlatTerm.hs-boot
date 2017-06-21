module Serialise.Cborg.FlatTerm where

import {-# SOURCE #-} Serialise.Cborg.Encoding

type FlatTerm = [TermToken]

data TermToken
instance Show TermToken

toFlatTerm :: Encoding -> FlatTerm
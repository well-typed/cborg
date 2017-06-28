{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Macro.PkgAesonTH where

import Macro.Types
import Data.Aeson as Aeson
import Data.Aeson.TH as Aeson
import Data.ByteString.Lazy as BS
import Data.Maybe

serialise :: [GenericPackageDescription] -> BS.ByteString
serialise pkgs = Aeson.encode pkgs

deserialise :: BS.ByteString -> [GenericPackageDescription]
deserialise = fromJust . Aeson.decode'

deriveJSON defaultOptions ''Version
deriveJSON defaultOptions ''PackageName
deriveJSON defaultOptions ''PackageId
deriveJSON defaultOptions ''VersionRange
deriveJSON defaultOptions ''Dependency
deriveJSON defaultOptions ''CompilerFlavor
deriveJSON defaultOptions ''License
deriveJSON defaultOptions ''SourceRepo
deriveJSON defaultOptions ''RepoKind
deriveJSON defaultOptions ''RepoType
deriveJSON defaultOptions ''BuildType
deriveJSON defaultOptions ''Library
deriveJSON defaultOptions ''Executable
deriveJSON defaultOptions ''TestSuite
deriveJSON defaultOptions ''TestSuiteInterface
deriveJSON defaultOptions ''TestType
deriveJSON defaultOptions ''Benchmark
deriveJSON defaultOptions ''BenchmarkInterface
deriveJSON defaultOptions ''BenchmarkType
deriveJSON defaultOptions ''BuildInfo
deriveJSON defaultOptions ''ModuleName
deriveJSON defaultOptions ''Language
deriveJSON defaultOptions ''Extension
deriveJSON defaultOptions ''KnownExtension
deriveJSON defaultOptions ''PackageDescription
deriveJSON defaultOptions ''OS
deriveJSON defaultOptions ''Arch
deriveJSON defaultOptions ''Flag
deriveJSON defaultOptions ''FlagName
deriveJSON defaultOptions ''CondTree
deriveJSON defaultOptions ''ConfVar
deriveJSON defaultOptions ''Condition
deriveJSON defaultOptions ''GenericPackageDescription

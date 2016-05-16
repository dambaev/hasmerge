
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import HAviSynth.AviSynth

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable

import Control.Monad
import Data.List

import ApplyClip
import MergeClips

main = undefined -- dummy

pluginGetName:: IO (Ptr CChar)
pluginGetName = newCString "Haskell Merge Plugin"
foreign export stdcall pluginGetName:: IO (Ptr CChar)

foreign export stdcall applyClipMain:: Ptr C'AVS_ScriptEnvironment
       -> Ptr C'AVS_Value
       -> Ptr ()
       -> IO (Ptr C'AVS_Value)
foreign export stdcall mergeClipsMain:: Ptr C'AVS_ScriptEnvironment
       -> Ptr C'AVS_Value
       -> Ptr ()
       -> IO (Ptr C'AVS_Value)



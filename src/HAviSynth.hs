
{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import HAviSynth.AviSynthFFI

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr

import System.Process

invert :: Ptr C'AVS_ScriptEnvironment
       -> Ptr C'AVS_Value
       -> Ptr ()
       -> IO (Ptr C'AVS_Value)
invert env args params = do
    createProcess (proc "notepad.exe" [] )
    withCString "it works!" $ \ptr->
        c'avs_new_value_error ptr

foreign export stdcall invert :: Ptr C'AVS_ScriptEnvironment
       -> Ptr C'AVS_Value
       -> Ptr ()
       -> IO (Ptr C'AVS_Value)


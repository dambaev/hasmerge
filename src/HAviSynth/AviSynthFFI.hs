{-# LINE 1 "HAviSynth\AviSynthFFI.hsc" #-}

{-# LINE 2 "HAviSynth\AviSynthFFI.hsc" #-}


{-# LINE 4 "HAviSynth\AviSynthFFI.hsc" #-}

module HAviSynth.AviSynthFFI where
import Foreign.Ptr (Ptr,FunPtr,plusPtr)
import Foreign.Ptr (wordPtrToPtr,castPtrToFunPtr)
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String (CString,CStringLen,CWString,CWStringLen)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (peekArray,pokeArray)
import Data.Int
import Data.Word

{-# LINE 7 "HAviSynth\AviSynthFFI.hsc" #-}


{-# LINE 9 "HAviSynth\AviSynthFFI.hsc" #-}

{-# LINE 10 "HAviSynth\AviSynthFFI.hsc" #-}

{-# LINE 11 "HAviSynth\AviSynthFFI.hsc" #-}

{-# LINE 12 "HAviSynth\AviSynthFFI.hsc" #-}

{-# LINE 13 "HAviSynth\AviSynthFFI.hsc" #-}

{-# LINE 14 "HAviSynth\AviSynthFFI.hsc" #-}

{-# LINE 15 "HAviSynth\AviSynthFFI.hsc" #-}

{-# LINE 16 "HAviSynth\AviSynthFFI.hsc" #-}

{-# LINE 17 "HAviSynth\AviSynthFFI.hsc" #-}
data C'AVS_Value = C'AVS_Value{
  c'AVS_Value'type :: CShort,
  c'AVS_Value'array_size :: CShort,
  c'AVS_Value'd'clip :: Ptr (),
  c'AVS_Value'd'boolean :: CChar,
  c'AVS_Value'd'integer :: CInt,
  c'AVS_Value'd'floating_pt :: CFloat,
  c'AVS_Value'd'string :: Ptr CChar,
  c'AVS_Value'd'array :: Ptr C'AVS_Value
} deriving (Eq,Show)
p'AVS_Value'type p = plusPtr p 0
p'AVS_Value'type :: Ptr (C'AVS_Value) -> Ptr (CShort)
p'AVS_Value'array_size p = plusPtr p 2
p'AVS_Value'array_size :: Ptr (C'AVS_Value) -> Ptr (CShort)
p'AVS_Value'd'clip p = plusPtr p 4
p'AVS_Value'd'clip :: Ptr (C'AVS_Value) -> Ptr (Ptr ())
p'AVS_Value'd'boolean p = plusPtr p 4
p'AVS_Value'd'boolean :: Ptr (C'AVS_Value) -> Ptr (CChar)
p'AVS_Value'd'integer p = plusPtr p 4
p'AVS_Value'd'integer :: Ptr (C'AVS_Value) -> Ptr (CInt)
p'AVS_Value'd'floating_pt p = plusPtr p 4
p'AVS_Value'd'floating_pt :: Ptr (C'AVS_Value) -> Ptr (CFloat)
p'AVS_Value'd'string p = plusPtr p 4
p'AVS_Value'd'string :: Ptr (C'AVS_Value) -> Ptr (Ptr CChar)
p'AVS_Value'd'array p = plusPtr p 4
p'AVS_Value'd'array :: Ptr (C'AVS_Value) -> Ptr (Ptr C'AVS_Value)
u'AVS_Value'd'clip :: C'AVS_Value -> Ptr () -> IO C'AVS_Value
u'AVS_Value'd'clip v vf = alloca $ \p -> do
  poke p v
  pokeByteOff p 4 vf
  vu <- peek p
  return $ v
    {c'AVS_Value'd'clip = c'AVS_Value'd'clip vu}
    {c'AVS_Value'd'boolean = c'AVS_Value'd'boolean vu}
    {c'AVS_Value'd'integer = c'AVS_Value'd'integer vu}
    {c'AVS_Value'd'floating_pt = c'AVS_Value'd'floating_pt vu}
    {c'AVS_Value'd'string = c'AVS_Value'd'string vu}
    {c'AVS_Value'd'array = c'AVS_Value'd'array vu}
u'AVS_Value'd'boolean :: C'AVS_Value -> CChar -> IO C'AVS_Value
u'AVS_Value'd'boolean v vf = alloca $ \p -> do
  poke p v
  pokeByteOff p 4 vf
  vu <- peek p
  return $ v
    {c'AVS_Value'd'clip = c'AVS_Value'd'clip vu}
    {c'AVS_Value'd'boolean = c'AVS_Value'd'boolean vu}
    {c'AVS_Value'd'integer = c'AVS_Value'd'integer vu}
    {c'AVS_Value'd'floating_pt = c'AVS_Value'd'floating_pt vu}
    {c'AVS_Value'd'string = c'AVS_Value'd'string vu}
    {c'AVS_Value'd'array = c'AVS_Value'd'array vu}
u'AVS_Value'd'integer :: C'AVS_Value -> CInt -> IO C'AVS_Value
u'AVS_Value'd'integer v vf = alloca $ \p -> do
  poke p v
  pokeByteOff p 4 vf
  vu <- peek p
  return $ v
    {c'AVS_Value'd'clip = c'AVS_Value'd'clip vu}
    {c'AVS_Value'd'boolean = c'AVS_Value'd'boolean vu}
    {c'AVS_Value'd'integer = c'AVS_Value'd'integer vu}
    {c'AVS_Value'd'floating_pt = c'AVS_Value'd'floating_pt vu}
    {c'AVS_Value'd'string = c'AVS_Value'd'string vu}
    {c'AVS_Value'd'array = c'AVS_Value'd'array vu}
u'AVS_Value'd'floating_pt :: C'AVS_Value -> CFloat -> IO C'AVS_Value
u'AVS_Value'd'floating_pt v vf = alloca $ \p -> do
  poke p v
  pokeByteOff p 4 vf
  vu <- peek p
  return $ v
    {c'AVS_Value'd'clip = c'AVS_Value'd'clip vu}
    {c'AVS_Value'd'boolean = c'AVS_Value'd'boolean vu}
    {c'AVS_Value'd'integer = c'AVS_Value'd'integer vu}
    {c'AVS_Value'd'floating_pt = c'AVS_Value'd'floating_pt vu}
    {c'AVS_Value'd'string = c'AVS_Value'd'string vu}
    {c'AVS_Value'd'array = c'AVS_Value'd'array vu}
u'AVS_Value'd'string :: C'AVS_Value -> Ptr CChar -> IO C'AVS_Value
u'AVS_Value'd'string v vf = alloca $ \p -> do
  poke p v
  pokeByteOff p 4 vf
  vu <- peek p
  return $ v
    {c'AVS_Value'd'clip = c'AVS_Value'd'clip vu}
    {c'AVS_Value'd'boolean = c'AVS_Value'd'boolean vu}
    {c'AVS_Value'd'integer = c'AVS_Value'd'integer vu}
    {c'AVS_Value'd'floating_pt = c'AVS_Value'd'floating_pt vu}
    {c'AVS_Value'd'string = c'AVS_Value'd'string vu}
    {c'AVS_Value'd'array = c'AVS_Value'd'array vu}
u'AVS_Value'd'array :: C'AVS_Value -> Ptr C'AVS_Value -> IO C'AVS_Value
u'AVS_Value'd'array v vf = alloca $ \p -> do
  poke p v
  pokeByteOff p 4 vf
  vu <- peek p
  return $ v
    {c'AVS_Value'd'clip = c'AVS_Value'd'clip vu}
    {c'AVS_Value'd'boolean = c'AVS_Value'd'boolean vu}
    {c'AVS_Value'd'integer = c'AVS_Value'd'integer vu}
    {c'AVS_Value'd'floating_pt = c'AVS_Value'd'floating_pt vu}
    {c'AVS_Value'd'string = c'AVS_Value'd'string vu}
    {c'AVS_Value'd'array = c'AVS_Value'd'array vu}
instance Storable C'AVS_Value where
  sizeOf _ = 8
  alignment _ = 4
  peek p = do
    v0 <- peekByteOff p 0
    v1 <- peekByteOff p 2
    v2 <- peekByteOff p 4
    v3 <- peekByteOff p 4
    v4 <- peekByteOff p 4
    v5 <- peekByteOff p 4
    v6 <- peekByteOff p 4
    v7 <- peekByteOff p 4
    return $ C'AVS_Value v0 v1 v2 v3 v4 v5 v6 v7
  poke p (C'AVS_Value v0 v1 v2 v3 v4 v5 v6 v7) = do
    pokeByteOff p 0 v0
    pokeByteOff p 2 v1
    pokeByteOff p 4 v2
    pokeByteOff p 4 v3
    pokeByteOff p 4 v4
    pokeByteOff p 4 v5
    pokeByteOff p 4 v6
    pokeByteOff p 4 v7
    return ()

{-# LINE 18 "HAviSynth\AviSynthFFI.hsc" #-}



data C'AVS_ScriptEnvironment = C'AVS_ScriptEnvironment

{-# LINE 22 "HAviSynth\AviSynthFFI.hsc" #-}
data C'AVS_Clip = C'AVS_Clip

{-# LINE 23 "HAviSynth\AviSynthFFI.hsc" #-}
data C'AVS_VideoInfo = C'AVS_VideoInfo

{-# LINE 24 "HAviSynth\AviSynthFFI.hsc" #-}


type C'AVS_ApplyFunc = FunPtr ( Ptr C'AVS_ScriptEnvironment -> C'AVS_Value -> Ptr () -> IO (C'AVS_Value))

{-# LINE 27 "HAviSynth\AviSynthFFI.hsc" #-}

foreign import ccall "inline_avs_invoke" c'avs_invoke
  :: Ptr C'AVS_ScriptEnvironment -> Ptr CChar -> Ptr C'AVS_Value -> Ptr (Ptr CChar) -> IO (Ptr C'AVS_Value)

{-# LINE 29 "HAviSynth\AviSynthFFI.hsc" #-}
foreign import ccall "inline_avs_release_value" c'avs_release_value
  :: Ptr C'AVS_Value -> IO ()

{-# LINE 30 "HAviSynth\AviSynthFFI.hsc" #-}

foreign import ccall "inline_avs_take_clip" c'avs_take_clip
  :: Ptr C'AVS_Value -> Ptr C'AVS_ScriptEnvironment -> IO (Ptr C'AVS_Clip)

{-# LINE 32 "HAviSynth\AviSynthFFI.hsc" #-}

foreign import ccall "inline_avs_as_bool" c'avs_as_bool
  :: Ptr C'AVS_Value -> IO CInt

{-# LINE 34 "HAviSynth\AviSynthFFI.hsc" #-}
foreign import ccall "inline_avs_as_string" c'avs_as_string
  :: Ptr C'AVS_Value -> IO (Ptr CChar)

{-# LINE 35 "HAviSynth\AviSynthFFI.hsc" #-}
foreign import ccall "inline_avs_as_float" c'avs_as_float
  :: Ptr C'AVS_Value -> IO CDouble

{-# LINE 36 "HAviSynth\AviSynthFFI.hsc" #-}
foreign import ccall "inline_avs_as_error" c'avs_as_error
  :: Ptr C'AVS_Value -> IO (Ptr CChar)

{-# LINE 37 "HAviSynth\AviSynthFFI.hsc" #-}
foreign import ccall "inline_avs_as_array" c'avs_as_array
  :: Ptr C'AVS_Value -> IO (Ptr C'AVS_Value)

{-# LINE 38 "HAviSynth\AviSynthFFI.hsc" #-}

foreign import ccall "inline_avs_array_size" c'avs_array_size
  :: Ptr C'AVS_Value -> IO CInt

{-# LINE 40 "HAviSynth\AviSynthFFI.hsc" #-}
foreign import ccall "inline_avs_array_elt" c'avs_array_elt
  :: Ptr C'AVS_Value -> CInt -> IO (Ptr C'AVS_Value)

{-# LINE 41 "HAviSynth\AviSynthFFI.hsc" #-}

foreign import ccall "inline_avs_as_int" c'avs_as_int
  :: Ptr C'AVS_Value -> IO CInt

{-# LINE 43 "HAviSynth\AviSynthFFI.hsc" #-}


foreign import ccall "inline_avs_defined" c'avs_defined
  :: Ptr C'AVS_Value -> IO CInt

{-# LINE 46 "HAviSynth\AviSynthFFI.hsc" #-}
foreign import ccall "inline_avs_is_clip" c'avs_is_clip
  :: Ptr C'AVS_Value -> IO CInt

{-# LINE 47 "HAviSynth\AviSynthFFI.hsc" #-}
foreign import ccall "inline_avs_is_bool" c'avs_is_bool
  :: Ptr C'AVS_Value -> IO CInt

{-# LINE 48 "HAviSynth\AviSynthFFI.hsc" #-}
foreign import ccall "inline_avs_is_int" c'avs_is_int
  :: Ptr C'AVS_Value -> IO CInt

{-# LINE 49 "HAviSynth\AviSynthFFI.hsc" #-}
foreign import ccall "inline_avs_is_float" c'avs_is_float
  :: Ptr C'AVS_Value -> IO CInt

{-# LINE 50 "HAviSynth\AviSynthFFI.hsc" #-}
foreign import ccall "inline_avs_is_string" c'avs_is_string
  :: Ptr C'AVS_Value -> IO CInt

{-# LINE 51 "HAviSynth\AviSynthFFI.hsc" #-}
foreign import ccall "inline_avs_is_array" c'avs_is_array
  :: Ptr C'AVS_Value -> IO CInt

{-# LINE 52 "HAviSynth\AviSynthFFI.hsc" #-}
foreign import ccall "inline_avs_is_error" c'avs_is_error
  :: Ptr C'AVS_Value -> IO CInt

{-# LINE 53 "HAviSynth\AviSynthFFI.hsc" #-}

foreign import ccall "inline_avs_new_value_bool" c'avs_new_value_bool
  :: Ptr C'AVS_Value -> CInt -> IO (Ptr C'AVS_Value )

{-# LINE 55 "HAviSynth\AviSynthFFI.hsc" #-}
foreign import ccall "inline_avs_new_value_int" c'avs_new_value_int
  :: Ptr C'AVS_Value -> CInt -> IO (Ptr C'AVS_Value )

{-# LINE 56 "HAviSynth\AviSynthFFI.hsc" #-}
foreign import ccall "inline_avs_new_value_string" c'avs_new_value_string
  :: Ptr C'AVS_Value -> Ptr CChar -> IO (Ptr C'AVS_Value )

{-# LINE 57 "HAviSynth\AviSynthFFI.hsc" #-}
foreign import ccall "inline_avs_new_value_float" c'avs_new_value_float
  :: Ptr C'AVS_Value -> CFloat -> IO (Ptr C'AVS_Value )

{-# LINE 58 "HAviSynth\AviSynthFFI.hsc" #-}
foreign import ccall "inline_avs_new_value_clip" c'avs_new_value_clip
  :: Ptr C'AVS_Value -> Ptr C'AVS_Clip -> IO (Ptr C'AVS_Value )

{-# LINE 59 "HAviSynth\AviSynthFFI.hsc" #-}
foreign import ccall "inline_avs_new_value_array" c'avs_new_value_array
  :: Ptr C'AVS_Value -> Ptr C'AVS_Value -> CInt -> IO (Ptr C'AVS_Value )

{-# LINE 60 "HAviSynth\AviSynthFFI.hsc" #-}
foreign import ccall "inline_avs_new_value_error" c'avs_new_value_error
  :: Ptr C'AVS_Value -> Ptr CChar -> IO (Ptr C'AVS_Value)

{-# LINE 61 "HAviSynth\AviSynthFFI.hsc" #-}
foreign import ccall "inline_new_value" c'new_value
  :: CInt -> IO (Ptr C'AVS_Value)

{-# LINE 62 "HAviSynth\AviSynthFFI.hsc" #-}

foreign import stdcall "avs_get_video_info" c'avs_get_video_info
  :: Ptr C'AVS_Clip -> IO (Ptr C'AVS_VideoInfo)
foreign import stdcall "&avs_get_video_info" p'avs_get_video_info
  :: FunPtr (Ptr C'AVS_Clip -> IO (Ptr C'AVS_VideoInfo))

{-# LINE 64 "HAviSynth\AviSynthFFI.hsc" #-}
foreign import ccall "inline_avs_num_frames" c'avs_num_frames
  :: Ptr C'AVS_VideoInfo -> IO CInt

{-# LINE 65 "HAviSynth\AviSynthFFI.hsc" #-}

foreign import ccall "inline_avs_add_function" c'avs_add_function
  :: Ptr C'AVS_ScriptEnvironment -> Ptr CChar -> Ptr CChar-> FunPtr ( Ptr C'AVS_ScriptEnvironment -> Ptr C'AVS_Value -> Ptr () -> IO (Ptr C'AVS_Value)) -> Ptr () -> IO CInt

{-# LINE 68 "HAviSynth\AviSynthFFI.hsc" #-}

foreign import ccall "inline_avs_filter_wrapper" c'avs_filter_wrapper
  :: FunPtr (Ptr C'AVS_ScriptEnvironment -> Ptr C'AVS_Value -> Ptr () -> IO (Ptr C'AVS_Value)) -> FunPtr (Ptr C'AVS_ScriptEnvironment -> Ptr C'AVS_Value -> Ptr () -> IO (Ptr C'AVS_Value))

{-# LINE 71 "HAviSynth\AviSynthFFI.hsc" #-}

{-#LANGUAGE BangPatterns #-}
{-#LANGUAGE TypeSynonymInstances #-}
{-#LANGUAGE FlexibleInstances #-}
module HAviSynth.AviSynth 
    ( -- module HAviSynth.AviSynthFFI
      fromRawValue
    , toRawValue
    , avs_invoke
    , avs_clip
    , avs_error
    , trim
    , invert
    , deleteFrame
    , clipGetFramesNum
    , unalignedSplice
    , avs_add_function
    , avs_blank_clip
    , AVS_Value(..)
    , C'AVS_Value
    , C'AVS_ScriptEnvironment
    , AVS_Clip
    , AVSValueable (..)
    )
where

import HAviSynth.AviSynthFFI

import Foreign.Ptr
import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.C.String
import Data.Either
import Control.Monad

data AVS_Value = AVS_ValueClip (Ptr C'AVS_Clip)
               | AVS_ValueBool Bool
               | AVS_ValueInt Int
               | AVS_ValueFloat Double
               | AVS_ValueString String
               | AVS_ValueError String
               | AVS_ValueArray [AVS_Value]
    deriving (Eq,Show)

class AVSValueable a where
    toAVSValue :: a-> AVS_Value


data AVS_Clip = AVS_Clip (Ptr C'AVS_Clip)
              | AVS_ClipError String
    deriving (Eq,Show)

instance AVSValueable AVS_Clip where
    toAVSValue (AVS_Clip ptr) = AVS_ValueClip ptr
    toAVSValue (AVS_ClipError v) = AVS_ValueError v
    
instance AVSValueable Int where
    toAVSValue int = AVS_ValueInt int

instance AVSValueable Bool where
    toAVSValue v = AVS_ValueBool v

instance AVSValueable Double where
    toAVSValue v = AVS_ValueFloat v

instance AVSValueable String where
    toAVSValue v = AVS_ValueString v

instance AVSValueable [AVS_Value] where
    toAVSValue v = avs_array v

ife:: IO Bool -> IO a -> IO a -> IO a
ife if_ then_ else_ = do
    val <- if_
    case val of
        True-> then_
        _ -> else_

toRawValue:: AVS_Value 
          -> IO (Ptr C'AVS_Value)
toRawValue val = do
    ptr <- avs_new_value 1
    toRawValuePtr ptr val

toRawValuePtr:: Ptr C'AVS_Value-> AVS_Value-> IO (Ptr C'AVS_Value)
toRawValuePtr ptr (AVS_ValueInt v) = do
    c'avs_new_value_int ptr (fromIntegral v)
toRawValuePtr ptr (AVS_ValueBool v) = do
    c'avs_new_value_bool ptr $! if v == False then 0 else 1
toRawValuePtr ptr (AVS_ValueString v) = do
    p <- newCString v
    c'avs_new_value_string ptr p
toRawValuePtr ptr (AVS_ValueError v) = do
    p <- newCString v
    c'avs_new_value_error ptr p
toRawValuePtr ptr (AVS_ValueFloat v) = do
    c'avs_new_value_float ptr (fromRational $ toRational  v)
toRawValuePtr ptr (AVS_ValueClip v) = do
    c'avs_new_value_clip ptr v
toRawValuePtr ptr (AVS_ValueArray v) = do
    avs_new_value_array ptr v

    
fromRawValue:: Ptr C'AVS_Value 
            -> Ptr C'AVS_ScriptEnvironment
            -> IO (AVS_Value)
fromRawValue ptr env = do
    defined <- avs_is_defined ptr
    if defined == False then return $! AVS_ValueError "AVS_Value undefined"
        else do
            tryClip $! tryInt $! tryBool $! tryString $! tryArray $!
                tryError $! tryFloat $!
                ( return $! AVS_ValueError "AVS_Value Unsupported")
            where
            tryInt = ife (avs_is_int ptr)
                ( avs_as_int ptr >>= return . AVS_ValueInt )
            tryClip = ife (avs_is_clip ptr)
                ( avs_take_clip ptr env >>= return . AVS_ValueClip )
            tryBool = ife (avs_is_bool ptr)
                ( avs_as_bool ptr >>= return .  AVS_ValueBool )
            tryFloat = ife (avs_is_float ptr)
                ( avs_as_float ptr  >>= return  . AVS_ValueFloat )
            tryString = ife (avs_is_string ptr)
                ( avs_as_string ptr >>= return  . AVS_ValueString )
            tryError = ife (avs_is_error ptr)
                ( avs_as_error ptr >>= return  . AVS_ValueError )
            tryArray = ife (avs_is_array ptr)
                ( parse_array ptr env >>= return . AVS_ValueArray )


parse_array:: Ptr C'AVS_Value 
            -> Ptr C'AVS_ScriptEnvironment
            -> IO [AVS_Value]
parse_array ptr env = do
    size <- avs_array_size ptr
    let helper !id size tmp | id >= size = return tmp
        helper !id size tmp = do
            elem <- avs_array_elt ptr id
            !newelem <- fromRawValue elem env
            helper (id+1) size (tmp++[newelem])
    helper 0 size []
    
avs_array_size:: Ptr C'AVS_Value-> IO Int
avs_array_size ptr = do
    ret <- c'avs_array_size ptr
    return $! fromIntegral ret


avs_array_elt:: Ptr C'AVS_Value-> Int-> IO (Ptr C'AVS_Value)
avs_array_elt ptr id = do
    c'avs_array_elt ptr (fromIntegral id)


avs_as_int:: Ptr C'AVS_Value-> IO Int
avs_as_int ptr = do
    ret <- c'avs_as_int ptr
    return $! fromIntegral ret

avs_as_bool:: Ptr C'AVS_Value-> IO Bool
avs_as_bool ptr = do
    ret <- c'avs_as_bool ptr
    return $! if fromIntegral ret == 0 
        then False
        else True

avs_take_clip:: Ptr C'AVS_Value -> Ptr C'AVS_ScriptEnvironment-> IO (Ptr C'AVS_Clip)
avs_take_clip ptr env = do
    c'avs_take_clip ptr env

avs_as_string:: Ptr C'AVS_Value-> IO String
avs_as_string ptr = do
    ret <- c'avs_as_string ptr
    peekCString ret

avs_as_float:: Ptr C'AVS_Value-> IO Double
avs_as_float ptr = do
    ret <- c'avs_as_float ptr
    return $! fromRational $! toRational ret

avs_as_error:: Ptr C'AVS_Value-> IO String
avs_as_error ptr = do
    ret <- c'avs_as_error ptr
    peekCString ret
    
avs_as_array:: Ptr C'AVS_Value-> IO (Ptr C'AVS_Value)
avs_as_array ptr = do
    c'avs_as_array ptr


avs_is_defined:: Ptr C'AVS_Value-> IO Bool
avs_is_defined ptr = do
    ret <- c'avs_defined ptr
    return $! if fromIntegral ret == 0 
        then False
        else True

avs_is_clip:: Ptr C'AVS_Value-> IO Bool
avs_is_clip ptr = do
    ret <- c'avs_is_clip ptr
    return $! if fromIntegral ret == 0 
        then False
        else True

avs_is_bool:: Ptr C'AVS_Value-> IO Bool
avs_is_bool ptr = do
    ret <- c'avs_is_bool ptr
    return $! if fromIntegral ret == 0 
        then False
        else True

avs_is_int:: Ptr C'AVS_Value-> IO Bool
avs_is_int ptr = do
    ret <- c'avs_is_int ptr
    return $! if fromIntegral ret == 0 
        then False
        else True

avs_is_float:: Ptr C'AVS_Value-> IO Bool
avs_is_float ptr = do
    ret <- c'avs_is_float ptr
    return $! if fromIntegral ret == 0 
        then False
        else True

avs_is_string:: Ptr C'AVS_Value-> IO Bool
avs_is_string ptr = do
    ret <- c'avs_is_string ptr
    return $! if fromIntegral ret == 0 
        then False
        else True

avs_is_array:: Ptr C'AVS_Value-> IO Bool
avs_is_array ptr = do
    ret <- c'avs_is_array ptr
    return $! if fromIntegral ret == 0 
        then False
        else True

avs_is_error:: Ptr C'AVS_Value-> IO Bool
avs_is_error ptr = do
    ret <- c'avs_is_error ptr
    return $! if fromIntegral ret == 0 
        then False
        else True


avs_new_value_array:: Ptr C'AVS_Value-> [AVS_Value]-> IO (Ptr C'AVS_Value)
avs_new_value_array p [] = c'avs_new_value_array p nullPtr 0
avs_new_value_array p ls = do
    let sz = length ls
        writeElems ptr id [] = return ()
        writeElems !ptr id (l:lls) = do
            toRawValuePtr ptr l
            let !newptr = ptr `plusPtr` sizeOf (undefined::C'AVS_Value)
                !newid = id + 1
            writeElems newptr newid lls
    !ptr <- avs_new_value sz
    writeElems ptr 0 ls
    c'avs_new_value_array p ptr (fromIntegral sz)

avs_new_value:: Int-> IO (Ptr C'AVS_Value)
avs_new_value sz = do
    let sz' = sz
    mallocArray sz'
    -- c'new_value (fromIntegral sz')

avs_invoke:: Ptr C'AVS_ScriptEnvironment-> String-> AVS_Value -> IO AVS_Value
avs_invoke env name args = do
    rawargs <- toRawValue args
    cname <- newCString name
    ret <- c'avs_invoke env cname rawargs nullPtr
    fromRawValue ret env 

avs_array:: [AVS_Value]-> AVS_Value
avs_array args = AVS_ValueArray args

avs_clip:: AVS_Value-> AVS_Clip
avs_clip (AVS_ValueClip ptr)= AVS_Clip ptr
avs_clip (AVS_ValueError v)= AVS_ClipError v

trim:: Ptr C'AVS_ScriptEnvironment
    -> AVS_Clip
    -> Int 
    -> Int
    -> IO AVS_Clip
trim env clip from to = do
    let args = avs_array [toAVSValue clip, AVS_ValueInt from
                         , AVS_ValueInt to]
    ret <- avs_invoke env "Trim" args
    return $! avs_clip ret

invert:: Ptr C'AVS_ScriptEnvironment
    -> AVS_Clip
    -> IO AVS_Clip
invert env clip = do
    liftM avs_clip $ avs_invoke env "Invert" $ avs_array [toAVSValue clip]
    
avs_error:: String-> AVS_Value
avs_error str = AVS_ValueError str

clipGetFramesNum:: AVS_Clip-> IO Int
clipGetFramesNum (AVS_Clip ptr) = do
    vi <- c'avs_get_video_info ptr
    fn <- c'avs_num_frames vi
    return $ fromIntegral fn
    
    
unalignedSplice:: Ptr C'AVS_ScriptEnvironment
    -> AVS_Clip
    -> AVS_Clip
    -> IO AVS_Clip
unalignedSplice env  begin end  = 
    liftM avs_clip $ avs_invoke env "UnalignedSplice" $ 
        avs_array [ toAVSValue begin, toAVSValue end]

deleteFrame:: Ptr C'AVS_ScriptEnvironment
    -> AVS_Clip
    -> Int
    -> IO AVS_Clip
deleteFrame env clip frame = do
    liftM avs_clip $ avs_invoke env "DeleteFrame" $
        avs_array [toAVSValue clip, toAVSValue frame] 


avs_add_function:: Ptr C'AVS_ScriptEnvironment
    -> String
    -> String
    -> FunPtr ( Ptr C'AVS_ScriptEnvironment
       -> Ptr C'AVS_Value
       -> Ptr ()
       -> IO (Ptr C'AVS_Value))
    -> IO Int
avs_add_function env name params foo = do
    cname <- newCString name
    cparams <- newCString params
    liftM fromIntegral $ c'avs_add_function env cname cparams 
        (c'avs_filter_wrapper foo) nullPtr
    
    
avs_blank_clip:: Ptr C'AVS_ScriptEnvironment
              -> AVS_Clip
              -> IO AVS_Clip
avs_blank_clip env clip = do
    liftM avs_clip $ avs_invoke env "BlankClip" $ avs_array $
        [toAVSValue clip, AVS_ValueInt 0]

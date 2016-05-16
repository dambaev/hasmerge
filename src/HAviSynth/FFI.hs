

module HAviSynth.FFI where


import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types
import Foreign.C

foreign import stdcall "avisynth_c.h avs_new_value_error" 
    c_avs_new_value_error:: CString-> IO CInt
    
avs_new_value_error:: String-> IO ()
avs_new_value_error err = do
    withCString err $ \ptr -> do
        _ <- c_avs_new_value_error ptr
        return ()

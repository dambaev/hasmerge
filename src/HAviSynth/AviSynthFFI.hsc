#include <bindings.dsl.h>

#include "avisynth_c.h"

module HAviSynth.AviSynthFFI where
#strict_import

#starttype AVS_Value
#field type , CShort
#field array_size , CShort
#union_field d.clip , Ptr ()
#union_field d.boolean , CChar
#union_field d.integer , CInt
#union_field d.floating_pt , CFloat
#union_field d.string , Ptr CChar
#union_field d.array , Ptr <AVS_Value>
#stoptype



#opaque_t AVS_ScriptEnvironment
#opaque_t AVS_Clip
#opaque_t AVS_VideoInfo


#synonym_t AVS_ApplyFunc , FunPtr ( Ptr <AVS_ScriptEnvironment> -> <AVS_Value> -> Ptr () -> IO (<AVS_Value>))

#cinline avs_invoke , Ptr <AVS_ScriptEnvironment> -> Ptr CChar -> Ptr <AVS_Value> -> Ptr (Ptr CChar) -> IO (Ptr <AVS_Value>)
#cinline avs_release_value , Ptr <AVS_Value> -> IO ()

#cinline avs_take_clip , Ptr <AVS_Value> -> Ptr <AVS_ScriptEnvironment> -> IO (Ptr <AVS_Clip>)

#cinline avs_as_bool , Ptr <AVS_Value> -> IO CInt
#cinline avs_as_string , Ptr <AVS_Value> -> IO (Ptr CChar)
#cinline avs_as_float , Ptr <AVS_Value> -> IO CDouble
#cinline avs_as_error , Ptr <AVS_Value> -> IO (Ptr CChar)
#cinline avs_as_array , Ptr <AVS_Value> -> IO (Ptr <AVS_Value>)

#cinline avs_array_size , Ptr <AVS_Value> -> IO CInt
#cinline avs_array_elt , Ptr <AVS_Value> -> CInt -> IO (Ptr <AVS_Value>)

#cinline avs_as_int , Ptr <AVS_Value> -> IO CInt


#cinline avs_defined , Ptr <AVS_Value> -> IO CInt
#cinline avs_is_clip , Ptr <AVS_Value> -> IO CInt
#cinline avs_is_bool , Ptr <AVS_Value> -> IO CInt
#cinline avs_is_int , Ptr <AVS_Value> -> IO CInt
#cinline avs_is_float , Ptr <AVS_Value> -> IO CInt
#cinline avs_is_string , Ptr <AVS_Value> -> IO CInt
#cinline avs_is_array , Ptr <AVS_Value> -> IO CInt
#cinline avs_is_error , Ptr <AVS_Value> -> IO CInt

#cinline avs_new_value_bool , Ptr <AVS_Value> -> CInt -> IO (Ptr <AVS_Value> )
#cinline avs_new_value_int , Ptr <AVS_Value> -> CInt -> IO (Ptr <AVS_Value> )
#cinline avs_new_value_string , Ptr <AVS_Value> -> Ptr CChar -> IO (Ptr <AVS_Value> )
#cinline avs_new_value_float , Ptr <AVS_Value> -> CFloat -> IO (Ptr <AVS_Value> )
#cinline avs_new_value_clip , Ptr <AVS_Value> -> Ptr <AVS_Clip> -> IO (Ptr <AVS_Value> )
#cinline avs_new_value_array , Ptr <AVS_Value> -> Ptr <AVS_Value> -> CInt -> IO (Ptr <AVS_Value> )
#cinline avs_new_value_error , Ptr <AVS_Value> -> Ptr CChar -> IO (Ptr <AVS_Value>)
#cinline new_value , CInt -> IO (Ptr <AVS_Value>)

#callconv avs_get_video_info , stdcall , Ptr <AVS_Clip> -> IO (Ptr <AVS_VideoInfo>)
#cinline avs_num_frames , Ptr <AVS_VideoInfo> -> IO CInt

#cinline avs_add_function , Ptr <AVS_ScriptEnvironment> -> Ptr CChar -> \
    Ptr CChar-> FunPtr ( Ptr <AVS_ScriptEnvironment> -> Ptr <AVS_Value> -> Ptr () -> IO (Ptr <AVS_Value>)) -> Ptr () -> IO CInt

#cinline avs_filter_wrapper , FunPtr (Ptr <AVS_ScriptEnvironment> -> Ptr <AVS_Value> -> Ptr () -> IO (Ptr <AVS_Value>)) -> \
    FunPtr (Ptr <AVS_ScriptEnvironment> -> Ptr <AVS_Value> -> Ptr () -> IO (Ptr <AVS_Value>))

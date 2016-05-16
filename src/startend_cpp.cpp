

#include "startend.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "Main_stub.h"

void AVSC_CC havisynth_at_exit(void* user_data, AVS_ScriptEnvironment * env)
{
    printf("haskel_deinit\n");
    //haskell_deinit();
}


AVS_Value AVSC_CC c_applyClip(AVS_ScriptEnvironment* env, AVS_Value args, void* param)
{
	AVS_Value retval;
	AVS_Value * ptr = 0;
	ptr = (AVS_Value * )applyClipMain( env, &args, param);
	if (ptr)
	{
	    retval = *ptr;
	    free (ptr);
	}
	printf("applyClip done\n");
	return retval;
}

AVS_Value AVSC_CC c_mergeClips(AVS_ScriptEnvironment* env, AVS_Value args, void* param)
{
	AVS_Value retval;
	AVS_Value * ptr = 0;
	ptr = (AVS_Value * )mergeClipsMain( env, &args, param);
	if (ptr)
	{
	    retval = *ptr;
	    free (ptr);
	}
	printf("mergeClips done\n");
	return retval;
}

AVSC_EXPORT
const char* AVSC_CC avisynth_c_plugin_init(AVS_ScriptEnvironment* env)
{
    printf("haskell_init\n");
    haskell_init();
    avs_at_exit(env, havisynth_at_exit, 0);
    avs_add_function(env, "ApplyClip", "ccs", c_applyClip, 0);
    avs_add_function(env, "MergeClips", "ccs", c_mergeClips, 0);
    printf("haskell_init done\n");
    return (const char*)pluginGetName();
}



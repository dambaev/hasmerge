

#include <Rts.h>
#include "startend.h"
#include "bindings.cmacros.h"
#include "bindings.dsl.h"
#include "windows.h"

extern void __stginit_Main(void);


void haskell_deinit()
{
   hs_exit();
}

void haskell_init(void)
{
   int argc = 1;
   char* argv[] = {HS_PLUGIN_NAME, NULL}; // argv must end with NULL

   // Initialize Haskell runtime
   char** args = argv;
   hs_init(&argc, &args);

   // Tell Haskell about all root modules
   hs_add_root(__stginit_Main);
}


AVS_Value* inline_avs_invoke(AVS_ScriptEnvironment *env, const char * name, 
                              AVS_Value * args, const char** arg_names)
{
    AVS_Value * ptr = 0;
    ptr = malloc((sizeof (struct AVS_Value)));
    *ptr = avs_invoke(env, name, *args, arg_names);
    return ptr;
}

void inline_avs_release_value( AVS_Value * ptr)
{
    if(!ptr)
	return;
    avs_release_value(*ptr);
    free(ptr);
}

int inline_avs_array_size(AVS_Value *pv) 
{
    if (!pv)
	return 0;
    return avs_array_size(*pv);
}

AVS_Value * new_value(int c)
{
    return (AVS_Value *) malloc ((sizeof (struct AVS_Value))*c);
}

AVS_Value * inline_new_value(int c)
{
    return (AVS_Value *) malloc ((sizeof (struct AVS_Value))*c);
}

AVS_Value * inline_avs_array_elt(AVS_Value * v, int index) 
{
    AVS_Value * ptr = 0;
    ptr = new_value(1);
    if(v)
	*ptr = avs_array_elt( *v, index);
    return ptr;
}

int inline_avs_as_int(AVS_Value * v) 
{
    if(!v)
        return 0;
    return avs_as_int(*v);
} 

AVS_Clip * inline_avs_take_clip(AVS_Value * p, AVS_ScriptEnvironment * env)
{
    if(!p)
        return 0;
    return avs_take_clip(*p, env);
}

int inline_avs_as_bool(AVS_Value * v) 
{
    if(!v)
	return 0;
    return avs_as_bool(*v); 
}   
const char * inline_avs_as_string(AVS_Value * v) 
{ 
    if (!v)
	return 0;
    return avs_as_string(*v); 
}
double inline_avs_as_float(AVS_Value * v) 
{ 
    if(!v)
	return 0.0;
    return avs_as_float(*v);
}
const char * inline_avs_as_error(AVS_Value *v) 
{ 
    if(!v)
	return 0;
    return avs_as_error(*v); 
}
const AVS_Value * inline_avs_as_array(AVS_Value *v)
{
    if(!v)
	return 0;
    return avs_as_array(*v); 
}

int inline_avs_defined(AVS_Value * v)
{ 
    if(!v)return 0;
    return avs_defined(*v); 
}
int inline_avs_is_clip(AVS_Value *v) 
{
    if(!v)return 0;
    return avs_is_clip(*v); 
}
int inline_avs_is_bool(AVS_Value *v) 
{
    if(!v)return 0;
    return avs_is_bool(*v); 
}
int inline_avs_is_int(AVS_Value *v) 
{
    if(!v)return 0;
    return avs_is_int(*v); 
}
int inline_avs_is_float(AVS_Value *v) 
{
    if(!v)return 0;
    return avs_is_float(*v); 
}
int inline_avs_is_string(AVS_Value *v) 
{
    if(!v)return 0;
    return avs_is_string(*v); 
}
int inline_avs_is_array(AVS_Value *v) 
{
    if(!v)return 0;
    return avs_is_array(*v); 
}
int inline_avs_is_error(AVS_Value *v) 
{
    if(!v)return 0;
    return avs_is_error(*v); 
}

struct AVS_Value* inline_avs_new_value_error(AVS_Value * ptr, const char * v0) 
{
    if(ptr)
	*ptr = avs_new_value_error(v0);
    return ptr;
}

struct AVS_Value* inline_avs_new_value_bool(AVS_Value * ptr, int v0) 
{
    if(ptr)
	*ptr = avs_new_value_bool(v0);
    return ptr;
}

struct AVS_Value* inline_avs_new_value_int(AVS_Value * ptr, int v0) 
{
    if(ptr)
	*ptr = avs_new_value_int(v0);
    return ptr;
}
struct AVS_Value* inline_avs_new_value_string(AVS_Value * ptr, const char* v0) 
{
    if(ptr)
	*ptr = avs_new_value_string(v0);
    return ptr;
}
struct AVS_Value* inline_avs_new_value_float(AVS_Value * ptr, float v0) 
{
    if(ptr)
	*ptr = avs_new_value_float(v0);
    return ptr;
}
struct AVS_Value* inline_avs_new_value_clip(AVS_Value * ptr, AVS_Clip * v0) 
{
    if(ptr)
	*ptr = avs_new_value_clip(v0);
    return ptr;
}
struct AVS_Value* inline_avs_new_value_array(AVS_Value * ptr, AVS_Value * v0, int size) 
{
    int a = 0;
    if(ptr)
	*ptr = avs_new_value_array(v0, size);
    return ptr;
}

int inline_avs_num_frames( AVS_VideoInfo * p)
{
    if(!p)
	return 0;
    return p-> num_frames;
}

typedef AVS_Value * (AVSC_CC * AVS_ApplyFuncWrapper)
                        (AVS_ScriptEnvironment *, AVS_Value * args, void * user_data);

AVS_Value inline_avs_filter_wrapper(AVS_ApplyFuncWrapper foo, AVS_ScriptEnvironment* env, AVS_Value args, void* param)
{
	AVS_Value retval;
	AVS_Value * ptr = 0;
	ptr = (AVS_Value * )foo( env, &args, param);
	if (ptr)
	{
	    retval = *ptr;
	    free (ptr);
	}
	return retval;
}


int inline_avs_add_function(AVS_ScriptEnvironment * env, 
    const char * name, const char * params, AVS_ApplyFunc foo, void * p)
{
    avs_add_function( env, name, params, foo, p);
}


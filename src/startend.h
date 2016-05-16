#ifndef __STARTEND_H__
#define __STARTEND_H__

#ifdef __cplusplus
extern "C" {
#endif

#include "avisynth_c.h"

#define HS_PLUGIN_NAME "hasinvert.dll"
#define HS_PLUGIN_MODULE Main

void
    haskell_init();

void
    haskell_deinit();

#ifdef __cplusplus
}
#endif

#endif // __STARTEND_H__

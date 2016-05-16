#include "HsFFI.h"
#ifdef __cplusplus
extern "C" {
#endif
extern HsPtr __attribute__((__stdcall__)) pluginGetName(void);
extern HsPtr __attribute__((__stdcall__)) applyClipMain(HsPtr a1, HsPtr a2, HsPtr a3);
extern HsPtr __attribute__((__stdcall__)) mergeClipsMain(HsPtr a1, HsPtr a2, HsPtr a3);
#ifdef __cplusplus
}
#endif


// DDS VFO Tuning
// Example for FlashForth using External Cfunctions
// This Cfunction is called via Cddstuning.s
// 64bit calculations
// c IgorM 6/2015
// GNU GPL v3
// No warranties of any kind
// Provided as-is

#include <math.h>

#define tuningbits24 16777216LL   // for 24bits
#define tuningbits32 4294967296LL   // for 32bits
#define tuningbits48 281474976710656LL   // for 48bits

long long Cddstune (long ddsclock, long mult, long offs, long vco) {

long long ddstune = (long long)vco + (long long)offs;
ddstune = ddstune / (long long)mult;
ddstune = ddstune * tuningbits32;
ddstune = ddstune / (long long)ddsclock;

return ddstune;
}

// 180000000. 1. -8997500. 14255000. ddstuning 2drop ud. 125448836  ok<#,ram>

// 500000000. 9. -8997500. 1296200000. ddstuning 2drop ud. 1228553920  ok<#,ram>

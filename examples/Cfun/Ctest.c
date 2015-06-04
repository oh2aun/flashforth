// Calling user's C function
// IgorM 3.6.2015

#include <stddef.h>
#include <math.h>

float Ctest(unsigned short a, unsigned short b, unsigned int c, float x, float y)
{
    a = a + 100;
    b = b / a;
    c = c * b;
    x = sqrtf( (float)c - x );
    y = y / x;
    return y;    // 4.5894021E-1
}


// decimal 100 2000 5. $0fdb $4049 $0fdb $4049 Ctest hex ok<#,ram> fa36 3eea
// $0fdb $4049 2constant pi
// decimal 100 2000 5. pi pi Ctest hex => fa36 3eea 
// 0x3eeafa36 = 4.5894021E-1

// Calling user's C function
// Examples
// Mind we are in XC16
// 	int is 16bit
// 	long int is 32bit
// 	float is 32bit
//
// Note: you have to enable "Init data section" in the xc16-ld settings!
// IgorM (c) 6.6.2015



#include <stddef.h>
#include <math.h>

/*
  long int Ctest(int a, int b, long int c, long int d, long int e)
{
    a = a + 100;
    b = b / a;
    c = c * b;
    d = c - d;
    e = e * d;
    return e;    // -200000000
}
*/

// decimal -50 20000 10. 2000. -10000. Ctest   ok<#,ram> 54016 65230
// d. -20000000  ok<#,ram>


float Ctest(int a, int b, long int c, float x, float y)
{
    a = a + 100;
    b = b / a;
    c = c * b;
    x = sqrtf((float)c - x);
    y = y / x;
    return y;    // 4.5894021E-1
}

// decimal 100 2000 5. $0fdb $4049 $0fdb $4049 Ctest hex ok<#,ram> fa36 3eea
// $0fdb $4049 2constant pi
// decimal 100 2000 5. pi pi Ctest hex = 0x3eeafa36 = 4.5894021E-1


/* float Ctest(short a, short b, long int c, float x, float y)
{
    float temp;
    a = a + 100;
    b = b / a;
    c = c * b;
    x = sqrtf((float)c - x);
    temp = sinf(x);
    y = y / x * temp;
    return y; 
}
*/
// decimal 900 -20000 -5. pi pi Ctest hex  ok<$,ram> 5b81 be04
// -1.292553E-1



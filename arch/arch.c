#include <inttypes.h>
#include <string.h>
#include <unistd.h>
#include <limits.h>
#include <stddef.h>
#include <float.h>
#include <wchar.h>
#include <wctype.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/utsname.h>

// #include "../data/HsFFI.h"
typedef void *HsPtr;
typedef void (*HsFunPtr)(void);

#define SIGN(x) ((int)((x)(0 - 1) < 0) ? "True" : "False")
#define FLOAT(x) ((int)(((x)0.1) != 0) ? "PrimTypeFloating" : "PrimTypeIntegral")

#define ALIGN(x) (__alignof__ (x))
#define SIZE(x) fprintf(FP, "  PrimType {\n    primTypeName = \"%s\",\n    primTypeIsSigned = %s,\n    primTypeType = %s,\n    primTypeAlignmentOf = %i,\n    primTypeSizeOf = %i },\n", #x , SIGN(x), FLOAT(x), ALIGN(x), sizeof(x))
#define SIZEP(x) fprintf(FP, "  PrimType {\n    primTypeName = \"%s\",\n    primTypeIsSigned = %s,\n    primTypeType = PrimTypePointer,\n    primTypeAlignmentOf = %i,\n    primTypeSizeOf = %i },\n", #x , SIGN(x), ALIGN(x), sizeof(x))
#define CONST(x) printf("const %19s  0x%llx\n", #x , (long long)x)


int
main(int argc, char *argv[])
{
        struct utsname utsname;
        char *str;
        FILE *FP;
        uname(&utsname);

        str = malloc(strlen(utsname.machine) + 20);
        strcpy(str, utsname.machine);
        strcat(str, ".arch");

        //FP = fopen(str,"w");
        FP = stdout;

        fprintf(FP, "arch_%s = [\n", utsname.machine);

        SIZE(uint32_t);
        SIZE(int);
        SIZE(intmax_t);
        SIZE(int8_t);
        SIZE(int16_t);
        SIZE(int32_t);
        SIZE(int64_t);
        SIZE(intmax_t);
        SIZE(intptr_t);
        SIZE(unsigned int);
        SIZE(uint8_t);
        SIZE(uint16_t);
        SIZE(uint32_t);
        SIZE(uint64_t);
        SIZE(uintmax_t);
        SIZE(uintptr_t);
        SIZE(float);
        SIZE(double);
        SIZE(long double);
        SIZEP(HsPtr);
        SIZEP(HsFunPtr);
        SIZE(char);
        SIZE(short);
        SIZE(int);
        SIZE(unsigned int);
        SIZE(size_t);
        SIZE(ssize_t);
        SIZE(wchar_t);
        SIZE(wint_t);
        SIZE(ptrdiff_t);
        SIZE(time_t);
        SIZE(wctype_t);
        SIZE(off_t);
        fprintf(FP, "  PrimType {\n    primTypeName = \"void\",\n    primTypeIsSigned = False,\n    primTypeType = PrimTypeVoid,\n    primTypeAlignmentOf = 0,\n    primTypeSizeOf = 0 }\n  ]\n\n");

        fclose(FP);

        return 0;
}



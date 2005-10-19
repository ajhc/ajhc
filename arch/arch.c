#include <inttypes.h>
#include <string.h>
#include <unistd.h>
#include <limits.h>
#include <float.h>
#include <wchar.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/utsname.h>

#include "../data/HsFFI.h"

#define SIZE(x) fprintf(FP, "  (\"%s\", %i),\n", #x , sizeof(x))
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

        FP = fopen(str,"w");

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
        SIZE(HsPtr);
        SIZE(HsFunPtr);
        SIZE(char);
        SIZE(short);
        SIZE(int);
        SIZE(unsigned int);
        SIZE(size_t);
        SIZE(wchar_t);
        SIZE(wint_t);
        fprintf(FP, "  (\"void\",0)\n ]\n");

        fclose(FP);

        return 0;
}



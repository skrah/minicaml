/*
 * Copyright (c) 2015 Stefan Krah. All rights reserved.
 *
 * This file is distributed under the terms of the Q Public License
 * version 1.0.
 */


#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <gc.h>



#define UNUSED __attribute__((unused))


int MiniCaml_main(void);

typedef struct {
    int64_t env;   /* env pointer for closures */
    int64_t value;
} caml_value;

static caml_value consts[257];


/* Internal functions, no need for an environment pointer. */
void *
MiniCaml_alloc(ssize_t nmemb, size_t eltsize)
{
    void *ptr;
    size_t size;

    if (nmemb < 0) {
        fprintf(stderr, "runtime error: malloc: negative element count\n");
        exit(1);
    }

    if ((size_t)nmemb > SIZE_MAX / eltsize) {
        fprintf(stderr, "runtime error: malloc: overflow in multiplication\n");
        exit(1);
    }

    size = nmemb * eltsize;
    ptr = GC_malloc(size);
    if (ptr == NULL) {
        fprintf(stderr, "runtime error: malloc: out of memory\n");
        exit(1);
    }

    return ptr;
}

caml_value
MiniCaml_init_array(const caml_value nmemb, const caml_value init)
{
    caml_value *ptr;
    caml_value ret;
    ssize_t i;

    ptr = MiniCaml_alloc(nmemb.value, sizeof(init));
    for (i = 0; i < nmemb.value; i++) {
        ptr[i] = init;
    }

    ret.env = 0;
    ret.value = (int64_t)ptr;
    return ret;
}

/* Functions that can be called from MiniCaml. These functions need an unused
   closure environment parameter. */
caml_value
MiniCaml_strcmp(caml_value env UNUSED, const caml_value s, const caml_value t)
{
    caml_value ret = {0, INT64_MAX};
    ret.value = strcmp((char *)s.value, (char *)t.value);
    return ret;
}

caml_value
MiniCaml_getchar(caml_value env UNUSED)
{
    caml_value empty_str = {0, (int64_t)""};
    int i;

    i = getc(stdin);
    if (i == EOF) {
        return empty_str;
    }

    return consts[i];
}

void
MiniCaml_print_string(caml_value env UNUSED, const caml_value s)
{
    printf("%s", (char *)s.value);
}

void
MiniCaml_print_bool(caml_value env UNUSED, const caml_value x)
{
    printf("%s", x.value ? "true" : "false");
}

void
MiniCaml_print_i64(caml_value env UNUSED, const caml_value x)
{
    printf("%lu", x.value);
}

void
MiniCaml_print_float(caml_value env UNUSED, const caml_value x)
{
    double f;
    memcpy(&f, &x.value, 8);
    printf("%.11f", f);
}

void
MiniCaml_flush(caml_value env UNUSED)
{
    fflush(stdout);
}

caml_value
MiniCaml_ord(caml_value env UNUSED, const caml_value s)
{
    caml_value ret = {0, INT64_MAX};

    if (*(char *)s.value == '\0') {
        return ret;
    }

    ret.value = ((char *)s.value)[0];
    return ret;
}

caml_value
MiniCaml_chr(caml_value env UNUSED, const caml_value v)
{
    int64_t i = v.value;

    if (i < 0 || i >= 256) {
        printf("chr(%ld) out of range\n", i);
        exit(1);
    }

    return consts[i];
}

caml_value
MiniCaml_size(caml_value env UNUSED, const caml_value s)
{
    caml_value ret = {0, INT64_MAX};
    ret.value = strlen((char *)s.value);
    return ret;
}

caml_value
MiniCaml_concat(caml_value env UNUSED, const caml_value a, const caml_value b)
{
    int64_t alen = strlen((char *)a.value);
    int64_t blen = strlen((char *)b.value);
    caml_value ret = {0, INT64_MAX};

    if (alen == 0) {
        return b;
    }
    else if (blen == 0) {
        return a;
    }
    else {
        int64_t n = alen + blen;
        char *t = MiniCaml_alloc(1, n);
        int64_t i;

        for (i = 0; i < alen; i++) {
            t[i] = ((char *)a.value)[i];
        }
        for(i = 0; i < blen; i++) {
            t[i + alen] = ((char *)b.value)[i];
        }

        ret.value = (int64_t)t;
        return ret;
    }
}

caml_value
MiniCaml_ref(caml_value env UNUSED, const caml_value x)
{
    caml_value nmemb = {0, 1};
    return MiniCaml_init_array(nmemb, x);
}

caml_value
MiniCaml_assign_ref(caml_value env UNUSED, caml_value lhs, const caml_value rhs)
{
    caml_value *ptr = (caml_value *)lhs.value;
    caml_value ret = {0, 0};
    *ptr = rhs;
    return ret;
}

caml_value
MiniCaml_deref(caml_value env UNUSED, caml_value ref)
{
    caml_value *ptr = (caml_value *)ref.value;
    return *ptr;
}


int
main(void)
{
    int i;
    char *cp;

    if (sizeof(uintptr_t) != 8 || sizeof(int64_t) != 8 ||
        sizeof(double) != 8 || sizeof(void *) != 8 ||
        sizeof(ssize_t) != 8) {
        fprintf(stderr, "MiniCaml: unexpected size requirements\n");
        exit(1);
    }

    for(i = 0; i < 256; i++) {
        caml_value v = {0, INT64_MAX};
        cp = MiniCaml_alloc(1, 2);
        snprintf(cp, 2, "%c", (unsigned char)i);
        v.value = (int64_t)cp;
        consts[i] = v;
    }

    return MiniCaml_main();
}





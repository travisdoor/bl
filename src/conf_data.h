// =================================================================================================
// bl
//
// File:   conf_data.h
// Author: Martin Dorazil
// Date:   7/23/19
//
// Copyright 2019 Martin Dorazil
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
// =================================================================================================

#ifndef BL_CONF_DATA_H
#define BL_CONF_DATA_H

#include "common.h"

enum conf_data_value_kind {
    CDV_UNKNOWN,
    CDV_STRING,
    CDV_INT,
};

struct conf_data_value {
    enum conf_data_value_kind kind;
    union {
        const char *v_str;
        int         v_int;
    };
};

typedef struct {
    struct {
        hash_t                 key;
        struct conf_data_value value;
    } * values;
    struct string_cache *cache;
} conf_data_t;

conf_data_t *conf_data_new();
void         conf_data_delete(conf_data_t *data);
void         conf_data_init(conf_data_t *data);
void         conf_data_terminate(conf_data_t *data);

bool conf_data_has_key(conf_data_t *data, const char *key);
void conf_data_add(conf_data_t *data, const char *key, struct conf_data_value *value);
struct conf_data_value *conf_data_get(conf_data_t *data, const char *key);

// Returned string is owned by conf_data.
const char *conf_data_get_str(conf_data_t *data, const char *key);
int         conf_data_get_int(conf_data_t *data, const char *key);

#endif

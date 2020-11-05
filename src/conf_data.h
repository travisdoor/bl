//************************************************************************************************
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
//************************************************************************************************

#ifndef BL_CONF_DATA_H
#define BL_CONF_DATA_H

#include "common.h"

typedef enum ConfDataValueKind {
    CDV_UNKNOWN,
    CDV_STRING,
    CDV_INT,
} ConfDataValueKind;

typedef struct ConfDataValue {
    union {
        const char *v_str;
        int         v_int;
    } data;

    ConfDataValueKind kind;
} ConfDataValue;

typedef THashTable ConfData;

void           conf_data_init(ConfData *data);
void           conf_data_terminate(ConfData *data);
void           conf_data_clear(ConfData *data);
bool           conf_data_has_key(ConfData *data, const char *key);
void           conf_data_add(ConfData *data, const char *key, ConfDataValue *value);
ConfDataValue *conf_data_get(ConfData *data, const char *key);
const char *   conf_data_get_str(ConfData *data, const char *key);
int            conf_data_get_int(ConfData *data, const char *key);

#endif

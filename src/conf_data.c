// =================================================================================================
// bl
//
// File:   conf_data.c
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

#include "conf_data.h"
#include "stb_ds.h"

conf_data_t *conf_data_new()
{
    conf_data_t *conf = bmalloc(sizeof(conf_data_t));
    conf_data_init(conf);
    return conf;
}

void conf_data_delete(conf_data_t *data)
{
    conf_data_terminate(data);
    bfree(data);
}

void conf_data_init(conf_data_t *data)
{
    data->cache  = NULL;
    data->values = NULL;
}

void conf_data_terminate(conf_data_t *data)
{
    hmfree(data->values);
    scfree(&data->cache);
}

bool conf_data_has_key(conf_data_t *data, const char *key)
{
    const hash_t hash = strhash(key);
    return hmgeti(data->values, hash) != -1;
}

void conf_data_add(conf_data_t *data, const char *key, struct conf_data_value *value)
{
    const hash_t hash = strhash(key);
    hmput(data->values, hash, *value);
}

struct conf_data_value *conf_data_get(conf_data_t *data, const char *key)
{
    const hash_t hash  = strhash(key);
    const s64    index = hmgeti(data->values, hash);
    if (index == -1) babort("Missing conf entry '%s'.", key);
    return &data->values[index].value;
}

const char *conf_data_get_str(conf_data_t *data, const char *key)
{
    struct conf_data_value *value = conf_data_get(data, key);
    if (value->kind != CDV_STRING) babort("Invalid type of conf value '%s', expected is string.");
    return value->v_str;
}

int conf_data_get_int(conf_data_t *data, const char *key)
{
    struct conf_data_value *value = conf_data_get(data, key);
    if (value->kind != CDV_INT) babort("Invalid type of conf value '%s', expected is int.");
    return value->v_int;
}

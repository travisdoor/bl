//************************************************************************************************
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
//************************************************************************************************

#include "conf_data.h"
#include "bobject/containers/hash.h"

ConfData *
conf_data_new(void)
{
	return bo_htbl_new(sizeof(ConfDataValue), 32);
}

void
conf_data_delete(ConfData *data)
{
	bo_unref(data);
}

bool
conf_data_has_key(ConfData *data, const char *key)
{
	const uint64_t hash = bo_hash_from_str(key);
	return bo_htbl_has_key(data, hash);
}

void
conf_data_add(ConfData *data, const char *key, ConfDataValue *value)
{
	const uint64_t hash = bo_hash_from_str(key);
	bo_htbl_insert(data, hash, *value);
}

ConfDataValue *
conf_data_get(ConfData *data, const char *key)
{
	const uint64_t hash = bo_hash_from_str(key);
	bo_iterator_t  it   = bo_htbl_find(data, hash);
	bo_iterator_t  end  = bo_htbl_end(data);

	if (bo_iterator_equal(&it, &end)) {
		bl_abort("Missing conf entry '%s'.", key);
	}

	return &bo_htbl_iter_peek_value(data, &it, ConfDataValue);
}

const char *
conf_data_get_str(ConfData *data, const char *key)
{
	ConfDataValue *value = conf_data_get(data, key);
	if (value->kind != CDV_STRING)
		bl_abort("Invalid type of conf value '%s', expected is string.");
	return value->v_str;
}

int
conf_data_get_int(ConfData *data, const char *key)
{
	ConfDataValue *value = conf_data_get(data, key);
	if (value->kind != CDV_INT)
		bl_abort("Invalid type of conf value '%s', expected is int.");
	return value->v_int;
}
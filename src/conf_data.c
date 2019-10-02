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

ConfData *
conf_data_new(void)
{
	return thtbl_new(sizeof(ConfDataValue), 32);
}

void
conf_data_delete(ConfData *data)
{
	thtbl_delete(data);
}

bool
conf_data_has_key(ConfData *data, const char *key)
{
	const u64 hash = thash_from_str(key);
	return thtbl_has_key(data, hash);
}

void
conf_data_add(ConfData *data, const char *key, ConfDataValue *value)
{
	const u64 hash = thash_from_str(key);
	thtbl_insert(data, hash, *value);
}

ConfDataValue *
conf_data_get(ConfData *data, const char *key)
{
	const u64     hash = thash_from_str(key);
	TIterator it   = thtbl_find(data, hash);
	TIterator end  = thtbl_end(data);

	if (TITERATOR_EQUAL(it, end)) {
		BL_ABORT("Missing conf entry '%s'.", key);
	}

	return &thtbl_iter_peek_value(ConfDataValue, it);
}

const char *
conf_data_get_str(ConfData *data, const char *key)
{
	ConfDataValue *value = conf_data_get(data, key);
	if (value->kind != CDV_STRING)
		BL_ABORT("Invalid type of conf value '%s', expected is string.");
	return value->data.v_str;
}

int
conf_data_get_int(ConfData *data, const char *key)
{
	ConfDataValue *value = conf_data_get(data, key);
	if (value->kind != CDV_INT) BL_ABORT("Invalid type of conf value '%s', expected is int.");
	return value->data.v_int;
}

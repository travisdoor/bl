// =================================================================================================
// bl
//
// File:   conf.c
// Author: Martin Dorazil
// Date:   12/19/21
//
// Copyright 2021 Martin Dorazil
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

#include "conf.h"
#include "builder.h"
#include "stb_ds.h"
#include <yaml.h>

// =================================================================================================
// Generic configuration loading
// =================================================================================================
struct entry {
    hash_t      key;
    const char *value;
};

struct config {
    struct entry        *data;
    struct string_cache *cache;
};

struct config *confload(const char *filepath)
{
    blog("Load config from '%s'.", filepath);

    yaml_parser_t parser;
    yaml_parser_initialize(&parser);

    FILE *input = fopen(filepath, "rb");
    // @Incomplete: handle errors

    yaml_parser_set_input_file(&parser, input);

    struct config *conf = bmalloc(sizeof(struct config));
    conf->data          = NULL;
    conf->cache         = NULL;

    enum state { STATE_KEY, STATE_VALUE } state = STATE_KEY;

    char blockpath[256] = "";

    struct entry entry;
    yaml_token_t token;
    char        *key  = gettmpstr();
    char        *path = gettmpstr();
    bool         done = false;
    while (!done) {
        if (!yaml_parser_scan(&parser, &token)) goto LOAD_ERROR;

        switch (token.type) {
        case YAML_STREAM_START_TOKEN:
            break;
        case YAML_KEY_TOKEN:
            state = STATE_KEY;
            break;
        case YAML_VALUE_TOKEN:
            state = STATE_VALUE;
            break;
        case YAML_BLOCK_MAPPING_START_TOKEN:
            if (strlenu(key)) {
                snprintf(blockpath, static_arrlenu(blockpath), "/%s", key);
            }
            break;
        case YAML_BLOCK_END_TOKEN:
            for (usize i = strlen(blockpath); i-- > 0;) {
                if (blockpath[i] == '/') {
                    blockpath[i] = '\0';
                    break;
                }
            }
            break;
        case YAML_SCALAR_TOKEN: {
            const char *value = (const char *)token.data.scalar.value;
            if (state == STATE_KEY) {
                strprint(key, "%s", value);
            } else {
                strprint(path, "%s/%s", blockpath, key);
                entry.key   = strhash(path);
                entry.value = scdup(&conf->cache, value, token.data.scalar.length);
                hmputs(conf->data, entry);
            }
            break;
        }
        case YAML_STREAM_END_TOKEN:
            done = true;
            break;
        default:
            bwarn("%s:%d:%d: YAML Unknown token type!",
                  filepath,
                  token.start_mark.line,
                  token.start_mark.column);
            break;
        }
        yaml_token_delete(&token);
    }
    yaml_parser_delete(&parser);
    fclose(input);
    puttmpstr(key);
    puttmpstr(path);
    return conf;

LOAD_ERROR:
    yaml_parser_delete(&parser);
    fclose(input);
    confdelete(conf);
    puttmpstr(key);
    puttmpstr(path);
    return NULL;
}

void confdelete(struct config *conf)
{
    if (!conf) return;
    hmfree(conf->data);
    scfree(&conf->cache);
    bfree(conf);
}

const char *confreads(struct config *conf, const char *path, const char *default_value)
{
    bassert(conf);
    const hash_t hash  = strhash(path);
    const s64    index = hmgeti(conf->data, hash);
    if (index == -1) {
        if (default_value) return default_value;
        babort("Unknown configuration entry '%s'!", path);
    }
    return conf->data[index].value;
}
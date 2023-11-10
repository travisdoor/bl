// =================================================================================================
// bl
//
// File:   x86_64.c
// Author: Martin Dorazil
// Date:   7/11/23
//
// Copyright 2023 Martin Dorazil
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

// Experimental x64 backend.

#include "assembly.h"
#include "bldebug.h"
#include "builder.h"
#include "common.h"
#include "stb_ds.h"
#include "threading.h"

struct context {
	char *foo;
};

static void emit_instr(struct context *ctx, struct mir_instr *instr);

void emit_instr(struct context *ctx, struct mir_instr *instr) {
	(void)instr;
}

static void job(struct job_context *ctx) {
	emit_instr(ctx->x64.ctx, ctx->x64.top_instr);
}

void x86_64run(struct assembly *assembly) {
	builder_warning("Using experimental x64 backend.");

	struct context ctx = {};

	if (builder.options->no_jobs) {
		babort("Not implemented.");
	} else {
		for (usize i = 0; i < arrlenu(assembly->MIR.exported_instrs); ++i) {
			struct job_context job_ctx = {.x64 = {.ctx = &ctx, .top_instr = assembly->MIR.exported_instrs[i]}};
			submit_job(&job, &job_ctx);
		}
		wait_threads();
	}
}

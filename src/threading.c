// =================================================================================================
// blc
//
// File:   threading.c
// Author: Martin Dorazil
// Date:   11/08/2023
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

#include "threading.h"
#include "stb_ds.h"

struct job {
	struct job_context ctx;
	job_fn_t           fn;
};

static array(struct job) jobs;
static pthread_mutex_t jobs_mutex;
static pthread_cond_t  jobs_cond;
static pthread_cond_t  working_cond;
static s32             jobs_running = 0;
static s32             thread_count = 0;
static bool            should_exit  = false;

static void *worker(void UNUSED(*args)) {
	bl_alloc_thread_init();

	while (true) {
		pthread_mutex_lock(&jobs_mutex);
		while (arrlenu(jobs) == 0 && !should_exit)
			pthread_cond_wait(&jobs_cond, &jobs_mutex);

		if (should_exit)
			break;

		struct job job;
		const bool has_job = arrlenu(jobs) > 0;
		if (has_job) {
			job = arrpop(jobs);
			bassert(arrlen(jobs) >= 0);
		}

		++jobs_running;
		bassert(jobs_running <= thread_count);
		pthread_mutex_unlock(&jobs_mutex);

		// Execute the job
		if (has_job)
			job.fn(&job.ctx);

		pthread_mutex_lock(&jobs_mutex);
		--jobs_running;
		bassert(jobs_running >= 0);

		// Might signal anyone waiting for the submitted batch to complete.
		if (!should_exit && jobs_running == 0 && arrlenu(jobs) == 0)
			pthread_cond_signal(&working_cond);
		pthread_mutex_unlock(&jobs_mutex);
	}

	bassert(thread_count > 0);

	--thread_count;
	pthread_cond_signal(&working_cond);
	pthread_mutex_unlock(&jobs_mutex);

	bl_alloc_thread_terminate();
	return NULL;
}

// =================================================================================================
// PUBLIC
// =================================================================================================

void threads_init(const s32 n) {
	bassert(n > 1);
	thread_count = n;

	pthread_mutex_init(&jobs_mutex, NULL);
	pthread_cond_init(&jobs_cond, NULL);
	pthread_cond_init(&working_cond, NULL);

	for (s32 i = 0; i < thread_count; ++i) {
		pthread_t thread;
		pthread_create(&thread, NULL, &worker, NULL);
		pthread_detach(thread);
	}
}

void threads_terminate(void) {
	pthread_mutex_lock(&jobs_mutex);
	arrsetlen(jobs, 0);
	should_exit = true;
	pthread_cond_broadcast(&jobs_cond);
	pthread_mutex_unlock(&jobs_mutex);

	wait_threads();

	pthread_cond_destroy(&working_cond);
	pthread_cond_destroy(&jobs_cond);
	pthread_mutex_destroy(&jobs_mutex);

	arrfree(jobs);
}

void wait_threads(void) {
	pthread_mutex_lock(&jobs_mutex);
	while (true) {
		if ((!should_exit && jobs_running != 0) || (should_exit && thread_count != 0)) {
			pthread_cond_wait(&working_cond, &jobs_mutex);
		} else {
			break;
		}
	}
	bassert(arrlenu(jobs) == 0);
	pthread_mutex_unlock(&jobs_mutex);
}

void submit_job(job_fn_t fn, struct job_context *ctx) {

	pthread_mutex_lock(&jobs_mutex);
	bassert(fn);
	struct job *job = arraddnptr(jobs, 1);
	if (ctx) {
		// Note in case we have no context, we leave the job's cxt uninitialized!
		memcpy(&job->ctx, ctx, sizeof(struct job_context));
	}
	job->fn = fn;

	pthread_cond_broadcast(&jobs_cond);
	pthread_mutex_unlock(&jobs_mutex);
}
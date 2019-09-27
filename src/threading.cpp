//************************************************************************************************
// bl
//
// File:   threading.cpp
// Author: Martin Dorazil
// Date:   9/17/19
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

#include "threading.h"
#include <thread>
#include <mutex>

#define CAST(T) reinterpret_cast<T>

using namespace std;

uint64_t
thread_get_id(void)
{
	return hash<thread::id>{}(this_thread::get_id());
}

Thread
thread_new(ThreadFn fn)
{
	return CAST(Thread)(new thread(fn));
}

void
thread_delete(Thread t)
{
	delete CAST(thread *)(t);
}

void
thread_join(Thread t)
{
	CAST(thread *)(t)->join();
}

Mutex
thread_mutex_new(void)
{
	return CAST(Mutex)(new mutex());
}

void
thread_mutex_delete(Mutex m)
{
	delete CAST(mutex *)(m);
}

void
thread_mutex_lock(Mutex m)
{
	CAST(mutex *)(m)->lock();
}

void
thread_mutex_unlock(Mutex m)
{
	CAST(mutex *)(m)->unlock();
}

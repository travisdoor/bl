#include <chrono>
#include <mutex>
#include <thread>
#include <stdlib.h>
#include "../Tracy.hpp"
#include "../common/TracySystem.hpp"

#define STB_IMAGE_IMPLEMENTATION
#define STBI_ONLY_JPEG
#include "stb_image.h"

struct static_init_test_t
{
    static_init_test_t()
    {
        ZoneScoped;
        new char[64*1024];
    }
};

static const static_init_test_t static_init_test;

void* operator new( std::size_t count )
{
    auto ptr = malloc( count );
    TracyAllocS( ptr, count, 10 );
    return ptr;
}

void operator delete( void* ptr ) noexcept
{
    TracyFreeS( ptr, 10 );
    free( ptr );
}

void TestFunction()
{
    tracy::SetThreadName( "First/second thread" );
    for(;;)
    {
        std::this_thread::sleep_for( std::chrono::milliseconds( 1 ) );
        ZoneScopedN( "Test function" );
        std::this_thread::sleep_for( std::chrono::milliseconds( 1 ) );
    }
}

void ResolutionCheck()
{
    tracy::SetThreadName( "Resolution check" );
    for(;;)
    {
        {
            ZoneScoped;
            std::this_thread::sleep_for( std::chrono::milliseconds( 1 ) );
        }
        {
            ZoneScoped;
            std::this_thread::sleep_for( std::chrono::milliseconds( 1 ) );
        }
    }

}

void ScopeCheck()
{
    tracy::SetThreadName( "Scope check" );
    for(;;)
    {
        std::this_thread::sleep_for( std::chrono::milliseconds( 1 ) );
        ZoneScoped;
    }
}

static TracyLockable( std::mutex, mutex );
static TracyLockable( std::recursive_mutex, recmutex );

void Lock1()
{
    tracy::SetThreadName( "Lock 1" );
    for(;;)
    {
        std::this_thread::sleep_for( std::chrono::milliseconds( 4 ) );
        std::lock_guard<LockableBase( std::mutex )> lock( mutex );
        LockMark( mutex );
        ZoneScoped;
        std::this_thread::sleep_for( std::chrono::milliseconds( 4 ) );
    }
}

void Lock2()
{
    tracy::SetThreadName( "Lock 2" );
    for(;;)
    {
        std::this_thread::sleep_for( std::chrono::milliseconds( 3 ) );
        std::unique_lock<LockableBase( std::mutex )> lock( mutex );
        LockMark( mutex );
        ZoneScoped;
        std::this_thread::sleep_for( std::chrono::milliseconds( 5 ) );
    }
}

void Lock3()
{
    tracy::SetThreadName( "Lock 3" );
    for(;;)
    {
        std::this_thread::sleep_for( std::chrono::milliseconds( 1 ) );
        std::unique_lock<LockableBase( std::mutex )> lock( mutex );
        LockMark( mutex );
        ZoneScoped;
        std::this_thread::sleep_for( std::chrono::milliseconds( 1 ) );
    }
}

void RecLock()
{
    tracy::SetThreadName( "Recursive mtx 1/2" );
    for(;;)
    {
        std::this_thread::sleep_for( std::chrono::milliseconds( 7 ) );
        std::lock_guard<LockableBase( std::recursive_mutex )> lock1( recmutex );
        TracyMessageL( "First lock" );
        LockMark( recmutex );
        ZoneScoped;
        {
            std::this_thread::sleep_for( std::chrono::milliseconds( 3 ) );
            std::lock_guard<LockableBase( std::recursive_mutex )> lock2( recmutex );
            TracyMessageL( "Second lock" );
            LockMark( recmutex );
            std::this_thread::sleep_for( std::chrono::milliseconds( 2 ) );
        }
    }
}

void Plot()
{
    tracy::SetThreadName( "Plot 1/2" );
    unsigned char i = 0;
    for(;;)
    {
        for( int j=0; j<1024; j++ )
        {
            TracyPlot( "Test plot", (int64_t)i++ );
        }
        std::this_thread::sleep_for( std::chrono::milliseconds( 1 ) );
    }
}

void MessageTest()
{
    tracy::SetThreadName( "Message test" );
    for(;;)
    {
        TracyMessage( "Tock", 4 );
        std::this_thread::sleep_for( std::chrono::milliseconds( 5 ) );
    }
}

static int Fibonacci( int n );

static inline int FibonacciInline( int n )
{
    return Fibonacci( n );
}

static int Fibonacci( int n )
{
    ZoneScoped;
    if( n < 2 ) return n;
    return FibonacciInline( n-1 ) + FibonacciInline( n-2 );
}

void DepthTest()
{
    tracy::SetThreadName( "Depth test" );
    for(;;)
    {
        std::this_thread::sleep_for( std::chrono::milliseconds( 20 ) );
        ZoneScoped;
        const auto txt = "Fibonacci (15)";
        ZoneText( txt, strlen( txt ) );
        Fibonacci( 15 );
    }
}

#ifdef __cpp_lib_shared_mutex

#include <shared_mutex>

static TracySharedLockable( std::shared_mutex, sharedMutex );

void SharedRead1()
{
    tracy::SetThreadName( "Shared read 1/2" );
    for(;;)
    {
        std::this_thread::sleep_for( std::chrono::milliseconds( 1 ) );
        std::shared_lock<SharedLockableBase( std::shared_mutex )> lock( sharedMutex );
        std::this_thread::sleep_for( std::chrono::milliseconds( 4 ) );
    }
}

void SharedRead2()
{
    tracy::SetThreadName( "Shared read 3" );
    for(;;)
    {
        std::this_thread::sleep_for( std::chrono::milliseconds( 6 ) );
        std::shared_lock<SharedLockableBase( std::shared_mutex )> lock( sharedMutex );
        std::this_thread::sleep_for( std::chrono::milliseconds( 1 ) );
    }
}

void SharedWrite1()
{
    tracy::SetThreadName( "Shared write 1" );
    for(;;)
    {
        std::this_thread::sleep_for( std::chrono::milliseconds( 3 ) );
        std::unique_lock<SharedLockableBase( std::shared_mutex )> lock( sharedMutex );
        std::this_thread::sleep_for( std::chrono::milliseconds( 2 ) );
    }
}

void SharedWrite2()
{
    tracy::SetThreadName( "Shared write 2" );
    for(;;)
    {
        std::this_thread::sleep_for( std::chrono::milliseconds( 5 ) );
        std::unique_lock<SharedLockableBase( std::shared_mutex )> lock( sharedMutex );
        std::this_thread::sleep_for( std::chrono::milliseconds( 1 ) );
    }
}

#endif

void CaptureCallstack()
{
    ZoneScopedS( 10 );
}

void CallstackTime()
{
    tracy::SetThreadName( "Callstack time" );
    for(;;)
    {
        std::this_thread::sleep_for( std::chrono::milliseconds( 1 ) );
        CaptureCallstack();
    }
}

void OnlyMemory()
{
    tracy::SetThreadName( "Only memory" );
    new int;
}

static TracyLockable( std::mutex, deadlockMutex1 );
static TracyLockable( std::mutex, deadlockMutex2 );

void DeadlockTest1()
{
    tracy::SetThreadName( "Deadlock test 1" );
    deadlockMutex1.lock();
    std::this_thread::sleep_for( std::chrono::milliseconds( 100 ) );
    deadlockMutex2.lock();
}

void DeadlockTest2()
{
    tracy::SetThreadName( "Deadlock test 2" );
    deadlockMutex2.lock();
    std::this_thread::sleep_for( std::chrono::milliseconds( 100 ) );
    deadlockMutex1.lock();
}

int main()
{
    auto t1 = std::thread( TestFunction );
    auto t2 = std::thread( TestFunction );
    auto t3 = std::thread( ResolutionCheck );
    auto t4 = std::thread( ScopeCheck );
    auto t5 = std::thread( Lock1 );
    auto t6 = std::thread( Lock2 );
    auto t7 = std::thread( Lock3 );
    auto t8 = std::thread( Plot );
    auto t9 = std::thread( Plot );
    auto t10 = std::thread( MessageTest );
    auto t11 = std::thread( DepthTest );
    auto t12 = std::thread( RecLock );
    auto t13 = std::thread( RecLock );
#ifdef __cpp_lib_shared_mutex
    auto t14 = std::thread( SharedRead1 );
    auto t15 = std::thread( SharedRead1 );
    auto t16 = std::thread( SharedRead2 );
    auto t17 = std::thread( SharedWrite1 );
    auto t18 = std::thread( SharedWrite2 );
#endif
    auto t19 = std::thread( CallstackTime );
    auto t20 = std::thread( OnlyMemory );
    auto t21 = std::thread( DeadlockTest1 );
    auto t22 = std::thread( DeadlockTest2 );

    int x, y;
    auto image = stbi_load( "image.jpg", &x, &y, nullptr, 4 );

    for(;;)
    {
        TracyMessageL( "Tick" );
        std::this_thread::sleep_for( std::chrono::milliseconds( 2 ) );
        {
            ZoneScoped;
            std::this_thread::sleep_for( std::chrono::milliseconds( 2 ) );
        }
        FrameImage( image, x, y, 0, false );
        FrameMark;
    }
}

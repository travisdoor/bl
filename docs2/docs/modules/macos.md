
# macos.bl

## macos.CFAllocatorRef

```c
CFAllocatorRef :: *u8
```



*File: macos.bl*


## macos.CFIndex

```c
CFIndex :: s64
```



*File: macos.bl*


## macos.CFStringRef

```c
CFStringRef :: *u8
```



*File: macos.bl*


## macos.CFArrayRef

```c
CFArrayRef :: *u8
```



*File: macos.bl*


## macos.CFArrayCallBacks

```c
CFArrayCallBacks :: u8
```



*File: macos.bl*


## macos.CFTimeInterval

```c
CFTimeInterval :: f64
```



*File: macos.bl*


## macos.CFTypeRef

```c
CFTypeRef :: *u8
```



*File: macos.bl*


## macos.CFRunLoopRef

```c
CFRunLoopRef :: *u8
```



*File: macos.bl*


## macos.CFRunLoopMode

```c
CFRunLoopMode :: CFStringRef
```



*File: macos.bl*


## macos.Boolean

```c
Boolean :: u8
```



*File: macos.bl*


## macos.CFStringEncoding

```c
CFStringEncoding :: enum u32 {
    MacRoman :: 0;
    WindowsLatin1 :: 1280;
    ISOLatin1 :: 513;
    NextStepLatin :: 2817;
    ASCII :: 1536;
    Unicode :: 256;
    UTF8 :: 134217984;
    NonLossyASCII :: 3071;
    UTF16 :: 256;
    UTF16BE :: 268435712;
    UTF16LE :: 335544576;
    UTF32 :: 201326848;
    UTF32BE :: 402653440;
    UTF32LE :: 469762304;
}
```



*File: macos.bl*


## macos.kFSEventStreamEventFlagNone

```c
kFSEventStreamEventFlagNone : u32 : 0
```



*File: macos.bl*


## macos.kFSEventStreamEventFlagMustScanSubDirs

```c
kFSEventStreamEventFlagMustScanSubDirs : u32 : 1
```



*File: macos.bl*


## macos.kFSEventStreamEventFlagUserDropped

```c
kFSEventStreamEventFlagUserDropped : u32 : 2
```



*File: macos.bl*


## macos.kFSEventStreamEventFlagKernelDropped

```c
kFSEventStreamEventFlagKernelDropped : u32 : 4
```



*File: macos.bl*


## macos.kFSEventStreamEventFlagEventIdsWrapped

```c
kFSEventStreamEventFlagEventIdsWrapped : u32 : 8
```



*File: macos.bl*


## macos.kFSEventStreamEventFlagHistoryDone

```c
kFSEventStreamEventFlagHistoryDone : u32 : 16
```



*File: macos.bl*


## macos.kFSEventStreamEventFlagRootChanged

```c
kFSEventStreamEventFlagRootChanged : u32 : 32
```



*File: macos.bl*


## macos.kFSEventStreamEventFlagMount

```c
kFSEventStreamEventFlagMount : u32 : 64
```



*File: macos.bl*


## macos.kFSEventStreamEventFlagUnmount

```c
kFSEventStreamEventFlagUnmount : u32 : 128
```



*File: macos.bl*


## macos.kFSEventStreamEventFlagItemCreated

```c
kFSEventStreamEventFlagItemCreated : u32 : 256
```



*File: macos.bl*


## macos.kFSEventStreamEventFlagItemRemoved

```c
kFSEventStreamEventFlagItemRemoved : u32 : 512
```



*File: macos.bl*


## macos.kFSEventStreamEventFlagItemInodeMetaMod

```c
kFSEventStreamEventFlagItemInodeMetaMod : u32 : 1024
```



*File: macos.bl*


## macos.kFSEventStreamEventFlagItemRenamed

```c
kFSEventStreamEventFlagItemRenamed : u32 : 2048
```



*File: macos.bl*


## macos.kFSEventStreamEventFlagItemModified

```c
kFSEventStreamEventFlagItemModified : u32 : 4096
```



*File: macos.bl*


## macos.kFSEventStreamEventFlagItemFinderInfoMod

```c
kFSEventStreamEventFlagItemFinderInfoMod : u32 : 8192
```



*File: macos.bl*


## macos.kFSEventStreamEventFlagItemChangeOwner

```c
kFSEventStreamEventFlagItemChangeOwner : u32 : 16384
```



*File: macos.bl*


## macos.kFSEventStreamEventFlagItemXattrMod

```c
kFSEventStreamEventFlagItemXattrMod : u32 : 32768
```



*File: macos.bl*


## macos.kFSEventStreamEventFlagItemIsFile

```c
kFSEventStreamEventFlagItemIsFile : u32 : 65536
```



*File: macos.bl*


## macos.kFSEventStreamEventFlagItemIsDir

```c
kFSEventStreamEventFlagItemIsDir : u32 : 131072
```



*File: macos.bl*


## macos.kFSEventStreamEventFlagItemIsSymlink

```c
kFSEventStreamEventFlagItemIsSymlink : u32 : 262144
```



*File: macos.bl*


## macos.kFSEventStreamEventFlagOwnEvent

```c
kFSEventStreamEventFlagOwnEvent : u32 : 524288
```



*File: macos.bl*


## macos.kFSEventStreamEventFlagItemIsHardlink

```c
kFSEventStreamEventFlagItemIsHardlink : u32 : 1048576
```



*File: macos.bl*


## macos.kFSEventStreamEventFlagItemIsLastHardlink

```c
kFSEventStreamEventFlagItemIsLastHardlink : u32 : 2097152
```



*File: macos.bl*


## macos.kFSEventStreamEventFlagItemCloned

```c
kFSEventStreamEventFlagItemCloned : u32 : 4194304
```



*File: macos.bl*


## macos.kFSEventStreamEventIdSinceNow

```c
kFSEventStreamEventIdSinceNow : FSEventStreamEventId : 18446744073709551615
```



*File: macos.bl*


## macos.kFSEventStreamCreateFlagNone

```c
kFSEventStreamCreateFlagNone : FSEventStreamCreateFlags : 0
```



*File: macos.bl*


## macos.kFSEventStreamCreateFlagUseCFTypes

```c
kFSEventStreamCreateFlagUseCFTypes : FSEventStreamCreateFlags : 1
```



*File: macos.bl*


## macos.kFSEventStreamCreateFlagNoDefer

```c
kFSEventStreamCreateFlagNoDefer : FSEventStreamCreateFlags : 2
```



*File: macos.bl*


## macos.kFSEventStreamCreateFlagWatchRoot

```c
kFSEventStreamCreateFlagWatchRoot : FSEventStreamCreateFlags : 4
```



*File: macos.bl*


## macos.kFSEventStreamCreateFlagIgnoreSelf

```c
kFSEventStreamCreateFlagIgnoreSelf : FSEventStreamCreateFlags : 8
```



*File: macos.bl*


## macos.kFSEventStreamCreateFlagFileEvents

```c
kFSEventStreamCreateFlagFileEvents : FSEventStreamCreateFlags : 16
```



*File: macos.bl*


## macos.kFSEventStreamCreateFlagMarkSelf

```c
kFSEventStreamCreateFlagMarkSelf : FSEventStreamCreateFlags : 32
```



*File: macos.bl*


## macos.kFSEventStreamCreateFlagUseExtendedData

```c
kFSEventStreamCreateFlagUseExtendedData : FSEventStreamCreateFlags : 64
```



*File: macos.bl*


## macos.FSEventStreamRef

```c
FSEventStreamRef :: *u8
```



*File: macos.bl*


## macos.FSEventStreamEventId

```c
FSEventStreamEventId :: u64
```



*File: macos.bl*


## macos.FSEventStreamCreateFlags

```c
FSEventStreamCreateFlags :: u32
```



*File: macos.bl*


## macos.FSEventStreamEventFlags

```c
FSEventStreamEventFlags :: u32
```



*File: macos.bl*


## macos.ConstFSEventStreamRef

```c
ConstFSEventStreamRef :: *u8
```



*File: macos.bl*


## macos.CFAllocatorRetainCallBack

```c
CFAllocatorRetainCallBack :: *fn (info: *u8) *u8
```



*File: macos.bl*


## macos.CFAllocatorReleaseCallBack

```c
CFAllocatorReleaseCallBack :: *fn (info: *u8) 
```



*File: macos.bl*


## macos.CFAllocatorCopyDescriptionCallBack

```c
CFAllocatorCopyDescriptionCallBack :: *fn (info: *u8) CFStringRef
```



*File: macos.bl*


## macos.CFArrayCreate

```c
CFArrayCreate :: fn (allocator: CFAllocatorRef, values: **u8, numValues: CFIndex, callBacks: *CFArrayCallBacks) CFArrayRef #extern
```



*File: macos.bl*


## macos.CFRunLoopGetCurrent

```c
CFRunLoopGetCurrent :: fn () CFRunLoopRef #extern
```



*File: macos.bl*


## macos.CFRunLoopCopyCurrentMode

```c
CFRunLoopCopyCurrentMode :: fn (rl: CFRunLoopRef) CFRunLoopMode #extern
```



*File: macos.bl*


## macos.CFRelease

```c
CFRelease :: fn (cf: CFTypeRef)  #extern
```



*File: macos.bl*


## macos.CFStringCreateWithCString

```c
CFStringCreateWithCString :: fn (alloc: CFAllocatorRef, cStr: *u8, encoding: CFStringEncoding) CFStringRef #extern
```



*File: macos.bl*


## macos.FSEventStreamContext

```c
FSEventStreamContext :: struct {
    version: CFIndex;
    info: *u8;
    retain: CFAllocatorRetainCallBack;
    release: CFAllocatorReleaseCallBack;
    copyDescription: CFAllocatorCopyDescriptionCallBack;
}
```



*File: macos.bl*


## macos.FSEventStreamCallback

```c
FSEventStreamCallback :: *fn (streamRef: ConstFSEventStreamRef, clientCallBackInfo: *u8, numEvents: usize, eventPaths: *u8, eventFlags: *FSEventStreamEventFlags, eventIds: *FSEventStreamEventId) 
```



*File: macos.bl*


## macos.FSEventStreamCreate

```c
FSEventStreamCreate :: fn (allocator: CFAllocatorRef, callback: FSEventStreamCallback, context: *FSEventStreamContext, pathsToWatch: CFArrayRef, sinceWhen: FSEventStreamEventId, latency: CFTimeInterval, flags: FSEventStreamCreateFlags) FSEventStreamRef #extern
```



*File: macos.bl*


## macos.FSEventStreamScheduleWithRunLoop

```c
FSEventStreamScheduleWithRunLoop :: fn (streamRef: FSEventStreamRef, runLoop: CFRunLoopRef, runLoopMode: CFStringRef)  #extern
```



*File: macos.bl*


## macos.FSEventStreamUnscheduleFromRunLoop

```c
FSEventStreamUnscheduleFromRunLoop :: fn (streamRef: FSEventStreamRef, runLoop: CFRunLoopRef, runLoopMode: CFStringRef)  #extern
```



*File: macos.bl*


## macos.FSEventStreamStart

```c
FSEventStreamStart :: fn (streamRef: FSEventStreamRef) Boolean #extern
```



*File: macos.bl*


## macos.FSEventStreamFlushSync

```c
FSEventStreamFlushSync :: fn (streamRef: FSEventStreamRef)  #extern
```



*File: macos.bl*


## macos.FSEventStreamStop

```c
FSEventStreamStop :: fn (streamRef: FSEventStreamRef)  #extern
```



*File: macos.bl*


## macos.FSEventStreamInvalidate

```c
FSEventStreamInvalidate :: fn (streamRef: FSEventStreamRef)  #extern
```



*File: macos.bl*


## macos.FSEventStreamRelease

```c
FSEventStreamRelease :: fn (streamRef: FSEventStreamRef)  #extern
```



*File: macos.bl*


## macos.NSGetExecutablePath

```c
NSGetExecutablePath :: fn (buf: *C.char, bufsize: *C.uint) C.int #extern
```



*File: macos.bl*


## macos.MachTimebaseInfo

```c
MachTimebaseInfo :: struct {
    numer: C.uint;
    denom: C.uint;
}
```



*File: macos.bl*


## macos.mach_timebase_info

```c
mach_timebase_info :: fn (info: *MachTimebaseInfo) C.int #extern
```



*File: macos.bl*


## macos.mach_absolute_time

```c
mach_absolute_time :: fn () u64 #extern
```



*File: macos.bl*


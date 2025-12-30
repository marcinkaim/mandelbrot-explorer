with Interfaces.C; use Interfaces.C;
with Interfaces;
with System;

package CUDA_Driver_API is
   pragma Preelaborate;

   ---------------------------------------------------------------------------
   -- 1. TYPE DEFINITIONS & CONSTANTS
   ---------------------------------------------------------------------------
   
   -- Error Handling
   type CUresult is new unsigned;
   CUDA_SUCCESS : constant CUresult := 0;

   -- Opaque Handles (Pointers to Driver Internal Structures)
   -- These reside in Host RAM, so System.Address is appropriate.
   type CUdevice    is new int;          -- Device ID is just an integer index
   type CUcontext   is new System.Address;
   type CUmodule    is new System.Address;
   type CUfunction  is new System.Address;
   type CUstream    is new System.Address;
   
   -- CUDA Graph Handles
   type CUgraph     is new System.Address;
   type CUgraphNode is new System.Address;
   type CUgraphExec is new System.Address;

   -- Device Pointer (VRAM Address)
   -- Physically a 64-bit virtual address on the GPU. 
   -- Treated as a numeric handle on Host to prevent accidental dereference.
   type CUdeviceptr is new Interfaces.Unsigned_64;
   
   -- Null Handle Constants
   No_Device_Ptr : constant CUdeviceptr := 0;

   -- Flags
   CU_CTX_SCHED_AUTO : constant unsigned := 0;

   ---------------------------------------------------------------------------
   -- 2. INITIALIZATION & CONTEXT MANAGEMENT
   ---------------------------------------------------------------------------

   -- Initialize the CUDA driver API.
   function cuInit (Flags : unsigned := 0) return CUresult;
   pragma Import (C, cuInit, "cuInit");

   -- Returns a handle to a compute device.
   function cuDeviceGet 
     (device  : access CUdevice; -- OUT
      ordinal : int) return CUresult;
   pragma Import (C, cuDeviceGet, "cuDeviceGet");

   -- Create a CUDA context.
   function cuCtxCreate 
     (pctx  : access CUcontext; -- OUT
      flags : unsigned; 
      dev   : CUdevice) return CUresult;
   pragma Import (C, cuCtxCreate, "cuCtxCreate");

   -- Destroy a CUDA context.
   function cuCtxDestroy (ctx : CUcontext) return CUresult;
   pragma Import (C, cuCtxDestroy, "cuCtxDestroy");

   -- Block until the context's tasks are completed.
   function cuCtxSynchronize return CUresult;
   pragma Import (C, cuCtxSynchronize, "cuCtxSynchronize");

   ---------------------------------------------------------------------------
   -- 3. MEMORY MANAGEMENT
   ---------------------------------------------------------------------------

   -- Allocate memory on the device.
   -- Returns the VRAM address into 'dptr'.
   function cuMemAlloc 
     (dptr     : access CUdeviceptr; -- OUT
      bytesize : size_t) return CUresult;
   pragma Import (C, cuMemAlloc, "cuMemAlloc");

   -- Free memory on the device.
   function cuMemFree (dptr : CUdeviceptr) return CUresult;
   pragma Import (C, cuMemFree, "cuMemFree");

   -- Copy memory from Host to Device.
   function cuMemcpyHtoD 
     (dstDevice : CUdeviceptr;   -- Handle (Value)
      srcHost   : System.Address; -- Pointer to Host Buffer
      ByteCount : size_t) return CUresult;
   pragma Import (C, cuMemcpyHtoD, "cuMemcpyHtoD");

   -- Copy memory from Device to Host.
   function cuMemcpyDtoH 
     (dstHost   : System.Address; -- Pointer to Host Buffer
      srcDevice : CUdeviceptr;   -- Handle (Value)
      ByteCount : size_t) return CUresult;
   pragma Import (C, cuMemcpyDtoH, "cuMemcpyDtoH");

   ---------------------------------------------------------------------------
   -- 4. MODULE & KERNEL EXECUTION (IMMEDIATE MODE)
   ---------------------------------------------------------------------------

   -- Load a compute module (PTX/CUBIN).
   function cuModuleLoad 
     (module : access CUmodule; -- OUT
      fname  : char_array) return CUresult;
   pragma Import (C, cuModuleLoad, "cuModuleLoad");

   -- Get a handle to a kernel function.
   function cuModuleGetFunction 
     (hfunc  : access CUfunction; -- OUT
      hmod   : CUmodule; 
      name   : char_array) return CUresult;
   pragma Import (C, cuModuleGetFunction, "cuModuleGetFunction");

   -- Launch a kernel.
   -- kernelParams: Array of pointers to arguments (System.Address)
   function cuLaunchKernel 
     (f              : CUfunction;
      gridDimX       : unsigned;
      gridDimY       : unsigned;
      gridDimZ       : unsigned;
      blockDimX      : unsigned;
      blockDimY      : unsigned;
      blockDimZ      : unsigned;
      sharedMemBytes : unsigned;
      hStream        : CUstream;
      kernelParams   : System.Address; 
      extra          : System.Address) return CUresult;
   pragma Import (C, cuLaunchKernel, "cuLaunchKernel");

   ---------------------------------------------------------------------------
   -- 5. CUDA GRAPHS API
   ---------------------------------------------------------------------------

   -- Kernel Node Parameters Structure
   -- Must match C struct layout strictly.
   type CUDA_KERNEL_NODE_PARAMS is record
      func           : CUfunction;
      gridDimX       : unsigned;
      gridDimY       : unsigned;
      gridDimZ       : unsigned;
      blockDimX      : unsigned;
      blockDimY      : unsigned;
      blockDimZ      : unsigned;
      sharedMemBytes : unsigned;
      kernelParams   : System.Address; -- void** (Array of pointers)
      extra          : System.Address;
   end record;
   pragma Convention (C, CUDA_KERNEL_NODE_PARAMS);

   -- Explicit Representation Clause to ensure alignment matches NVIDIA Driver
   for CUDA_KERNEL_NODE_PARAMS use record
      func           at 0  range 0 .. 63;
      gridDimX       at 8  range 0 .. 31;
      gridDimY       at 12 range 0 .. 31;
      gridDimZ       at 16 range 0 .. 31;
      blockDimX      at 20 range 0 .. 31;
      blockDimY      at 24 range 0 .. 31;
      blockDimZ      at 28 range 0 .. 31;
      sharedMemBytes at 32 range 0 .. 31;
      kernelParams   at 40 range 0 .. 63;
      extra          at 48 range 0 .. 63;
   end record;
   -- Total Size check: 56 bytes (64-bit aligned)
   
   -- Create a new empty graph.
   function cuGraphCreate 
     (phGraph : access CUgraph; -- OUT
      flags   : unsigned) return CUresult;
   pragma Import (C, cuGraphCreate, "cuGraphCreate");

   -- Destroy a graph.
   function cuGraphDestroy 
     (hGraph : CUgraph) return CUresult;
   pragma Import (C, cuGraphDestroy, "cuGraphDestroy");

   -- Add a kernel node to the graph.
   -- dependencies: Pointer to array of CUgraphNode handles (System.Address)
   function cuGraphAddKernelNode 
     (phGraphNode     : access CUgraphNode; -- OUT
      hGraph          : CUgraph;
      dependencies    : System.Address; 
      numDependencies : size_t;
      nodeParams      : access CUDA_KERNEL_NODE_PARAMS) return CUresult;
   pragma Import (C, cuGraphAddKernelNode, "cuGraphAddKernelNode");

   -- Create an executable graph from a graph template.
   function cuGraphInstantiate 
     (phGraphExec : access CUgraphExec; -- OUT
      hGraph      : CUgraph;
      pLogBuffer  : System.Address;     -- Optional error log buffer
      bufferSize  : size_t;
      flags       : unsigned) return CUresult;
   pragma Import (C, cuGraphInstantiate, "cuGraphInstantiate");

   -- Launch an executable graph.
   function cuGraphLaunch 
     (hGraphExec : CUgraphExec;
      hStream    : CUstream) return CUresult;
   pragma Import (C, cuGraphLaunch, "cuGraphLaunch");

   -- Destroy an executable graph.
   function cuGraphExecDestroy 
     (hGraphExec : CUgraphExec) return CUresult;
   pragma Import (C, cuGraphExecDestroy, "cuGraphExecDestroy");

end CUDA_Driver_API;
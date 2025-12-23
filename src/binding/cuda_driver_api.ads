with Interfaces.C;
with System;

package CUDA_Driver_API is
   pragma Preelaborate;

   use Interfaces.C;

   -- Error Handling
   type CUresult is new unsigned;
   CUDA_SUCCESS : constant CUresult := 0;
   
   -- Opaque Handles (Strong Typing Strategy)
   -- CUdevice is logically an integer index (0, 1, 2...)
   type CUdevice is new int;
   
   -- Pointers to opaque structs in C are mapped as System.Address, 
   -- but wrapped in distinct types to prevent mixing.
   type CUcontext is new System.Address;
   type CUmodule  is new System.Address;
   type CUfunction is new System.Address;
   type CUstream  is new System.Address;
   type CUdeviceptr is new System.Address; -- Unified virtual address space (64-bit)

   -- Flags
   CU_CTX_SCHED_AUTO : constant unsigned := 0;

   ---------------------------------------------------------------------------
   -- API Imports
   ---------------------------------------------------------------------------

   -- Initialize the CUDA driver API.
   -- Must be called before any other function.
   function cuInit (Flags : unsigned := 0) return CUresult;
   pragma Import (C, cuInit, "cuInit");

   -- Returns a handle to a compute device.
   function cuDeviceGet 
     (device  : access CUdevice; 
      ordinal : int) return CUresult;
   pragma Import (C, cuDeviceGet, "cuDeviceGet");

   -- Create a CUDA context.
   function cuCtxCreate 
     (pctx  : access CUcontext; 
      flags : unsigned; 
      dev   : CUdevice) return CUresult;
   pragma Import (C, cuCtxCreate, "cuCtxCreate");

   -- Destroy a CUDA context.
   function cuCtxDestroy (ctx : CUcontext) return CUresult;
   pragma Import (C, cuCtxDestroy, "cuCtxDestroy");

   -- Wait for context tasks to complete
   function cuCtxSynchronize return CUresult;
   pragma Import (C, cuCtxSynchronize, "cuCtxSynchronize");

   -- Simple memory allocation (Linear)
   function cuMemAlloc 
     (dptr      : access CUdeviceptr; 
      bytesize  : size_t) return CUresult;
   pragma Import (C, cuMemAlloc, "cuMemAlloc");

   -- Free memory
   function cuMemFree (dptr : CUdeviceptr) return CUresult;
   pragma Import (C, cuMemFree, "cuMemFree");

   ---------------------------------------------------------------------------
   -- Module & Execution Control
   ---------------------------------------------------------------------------

   -- Load a generic Compute Module (PTX/Cubin)
   function cuModuleLoad 
     (module : access CUmodule; 
      fname  : char_array) return CUresult;
   pragma Import (C, cuModuleLoad, "cuModuleLoad");

   -- Get a handle to a kernel function inside a loaded module
   function cuModuleGetFunction 
     (hfunc  : access CUfunction; 
      hmod   : CUmodule; 
      name   : char_array) return CUresult;
   pragma Import (C, cuModuleGetFunction, "cuModuleGetFunction");

   -- Launch a kernel on the GPU
   -- kernelParams: Array of pointers to arguments
   function cuLaunchKernel 
     (f             : CUfunction;
      gridDimX      : unsigned;
      gridDimY      : unsigned;
      gridDimZ      : unsigned;
      blockDimX     : unsigned;
      blockDimY     : unsigned;
      blockDimZ     : unsigned;
      sharedMemBytes: unsigned;
      hStream       : CUstream;
      kernelParams  : System.Address; 
      extra         : System.Address) return CUresult;
   pragma Import (C, cuLaunchKernel, "cuLaunchKernel");

   ---------------------------------------------------------------------------
   -- Memory Transfer (Host <-> Device)
   ---------------------------------------------------------------------------

   function cuMemcpyHtoD 
     (dstDevice : CUdeviceptr; 
      srcHost   : System.Address; 
      ByteCount : size_t) return CUresult;
   pragma Import (C, cuMemcpyHtoD, "cuMemcpyHtoD");

   function cuMemcpyDtoH 
     (dstHost   : System.Address; 
      srcDevice : CUdeviceptr; 
      ByteCount : size_t) return CUresult;
   pragma Import (C, cuMemcpyDtoH, "cuMemcpyDtoH");

   ---------------------------------------------------------------------------
   -- CUDA Graphs Types
   ---------------------------------------------------------------------------
   type CUgraph     is new System.Address;
   type CUgraphNode is new System.Address;
   type CUgraphExec is new System.Address;

   type CUDA_KERNEL_NODE_PARAMS is record
      func           : CUfunction;
      gridDimX       : unsigned;
      gridDimY       : unsigned;
      gridDimZ       : unsigned;
      blockDimX      : unsigned;
      blockDimY      : unsigned;
      blockDimZ      : unsigned;
      sharedMemBytes : unsigned;
      kernelParams   : System.Address; -- void** (tablica wskaźników)
      extra          : System.Address;
   end record;
   pragma Convention (C, CUDA_KERNEL_NODE_PARAMS);

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

   for CUDA_KERNEL_NODE_PARAMS'Size use 448;

   ---------------------------------------------------------------------------
   -- CUDA Graphs Functions
   ---------------------------------------------------------------------------

   function cuGraphCreate 
     (phGraph : access CUgraph; 
      flags   : unsigned) return CUresult;
   pragma Import (C, cuGraphCreate, "cuGraphCreate");

   function cuGraphDestroy 
     (hGraph : CUgraph) return CUresult;
   pragma Import (C, cuGraphDestroy, "cuGraphDestroy");

   function cuGraphAddKernelNode 
     (phGraphNode    : access CUgraphNode;
      hGraph         : CUgraph;
      dependencies   : System.Address; 
      numDependencies: size_t;
      nodeParams     : access CUDA_KERNEL_NODE_PARAMS) return CUresult;
   pragma Import (C, cuGraphAddKernelNode, "cuGraphAddKernelNode");

   function cuGraphInstantiate 
     (phGraphExec : access CUgraphExec;
      hGraph      : CUgraph;
      pLogBuffer  : System.Address; -- char*
      bufferSize  : size_t;
      flags       : unsigned) return CUresult;
   pragma Import (C, cuGraphInstantiate, "cuGraphInstantiate");

   function cuGraphLaunch 
     (hGraphExec : CUgraphExec;
      hStream    : CUstream) return CUresult;
   pragma Import (C, cuGraphLaunch, "cuGraphLaunch");

   function cuGraphExecDestroy 
     (hGraphExec : CUgraphExec) return CUresult;
   pragma Import (C, cuGraphExecDestroy, "cuGraphExecDestroy");

end CUDA_Driver_API;

#include <dvmhlib_cuda.h>
#define dcmplx2 Complex<double>
#define cmplx2 Complex<float>
typedef int __indexTypeInt; 
typedef long long __indexTypeLLong;





//--------------------- Kernel for loop on line 49 ---------------------

      __global__ void   loop_test2_49_cuda_kernel_int(float b[], __indexTypeInt b0002, float a[], __indexTypeInt a0002, __indexTypeInt begin_1, __indexTypeInt end_1, __indexTypeInt begin_2, __indexTypeInt end_2, __indexTypeInt blocks_1, __indexTypeInt add_blocks, int l)
      {

// Local needs
         int i;
         int j;
         __indexTypeInt rest_blocks, cur_blocks;

// Calculate each thread's loop variables' values
         rest_blocks = add_blocks + blockIdx.x;
         cur_blocks = rest_blocks / blocks_1;
         j = begin_1 + (cur_blocks * blockDim.y + threadIdx.y);
         if (j <= end_1) 
         {
            rest_blocks = rest_blocks - cur_blocks * blocks_1;
            cur_blocks = rest_blocks;
            i = begin_2 + (cur_blocks * blockDim.x + threadIdx.x);
            if (i <= end_2) 
            {

// Loop body
               a[i + a0002 * j] = 0.f;
               if (i == 1 | j == 1 | i == l | j == l) 
               {
                  b[i + b0002 * j] = 0.f;
               }
               else 
               {
                  b[i + b0002 * j] = 1.f + i + j;
               }
            }
         }
      }


//--------------------- Kernel for loop on line 49 ---------------------

      __global__ void   loop_test2_49_cuda_kernel_llong(float b[], __indexTypeLLong b0002, float a[], __indexTypeLLong a0002, __indexTypeLLong begin_1, __indexTypeLLong end_1, __indexTypeLLong begin_2, __indexTypeLLong end_2, __indexTypeLLong blocks_1, __indexTypeLLong add_blocks, int l)
      {

// Local needs
         int i;
         int j;
         __indexTypeLLong rest_blocks, cur_blocks;

// Calculate each thread's loop variables' values
         rest_blocks = add_blocks + blockIdx.x;
         cur_blocks = rest_blocks / blocks_1;
         j = begin_1 + (cur_blocks * blockDim.y + threadIdx.y);
         if (j <= end_1) 
         {
            rest_blocks = rest_blocks - cur_blocks * blocks_1;
            cur_blocks = rest_blocks;
            i = begin_2 + (cur_blocks * blockDim.x + threadIdx.x);
            if (i <= end_2) 
            {

// Loop body
               a[i + a0002 * j] = 0.f;
               if (i == 1 | j == 1 | i == l | j == l) 
               {
                  b[i + b0002 * j] = 0.f;
               }
               else 
               {
                  b[i + b0002 * j] = 1.f + i + j;
               }
            }
         }
      }


//--------------------- Kernel for loop on line 65 ---------------------

      __global__ void   loop_test2_65_cuda_kernel_int(float a[], __indexTypeInt a0002, float b[], __indexTypeInt b0002, float eps, float eps_grid[], __indexTypeInt begin_1, __indexTypeInt end_1, __indexTypeInt begin_2, __indexTypeInt end_2, __indexTypeInt blocks_1, __indexTypeInt add_blocks)
      {

// Local needs
         int i;
         int j;
         __indexTypeInt rest_blocks, cur_blocks;

// Calculate each thread's loop variables' values
         rest_blocks = add_blocks + blockIdx.x;
         cur_blocks = rest_blocks / blocks_1;
         j = begin_1 + (cur_blocks * blockDim.y + threadIdx.y);
         if (j <= end_1) 
         {
            rest_blocks = rest_blocks - cur_blocks * blocks_1;
            cur_blocks = rest_blocks;
            i = begin_2 + (cur_blocks * blockDim.x + threadIdx.x);
            if (i <= end_2) 
            {

// Loop body
               eps = max(eps, abs(b[i + b0002 * j] - a[i + a0002 * j]));
               a[i + a0002 * j] = b[i + b0002 * j];
            }
         }

// Reduction
         i = threadIdx.x + threadIdx.y * blockDim.x + threadIdx.z * (blockDim.x * blockDim.y);
         eps = __dvmh_blockReduceMax(eps);
         if (i % warpSize == 0) 
         {
            eps_grid[(add_blocks + blockIdx.x) * (blockDim.x * blockDim.y * blockDim.z / warpSize) + i / warpSize] = eps;
         }
      }


//--------------------- Kernel for loop on line 65 ---------------------

      __global__ void   loop_test2_65_cuda_kernel_llong(float a[], __indexTypeLLong a0002, float b[], __indexTypeLLong b0002, float eps, float eps_grid[], __indexTypeLLong begin_1, __indexTypeLLong end_1, __indexTypeLLong begin_2, __indexTypeLLong end_2, __indexTypeLLong blocks_1, __indexTypeLLong add_blocks)
      {

// Local needs
         int i;
         int j;
         __indexTypeLLong rest_blocks, cur_blocks;

// Calculate each thread's loop variables' values
         rest_blocks = add_blocks + blockIdx.x;
         cur_blocks = rest_blocks / blocks_1;
         j = begin_1 + (cur_blocks * blockDim.y + threadIdx.y);
         if (j <= end_1) 
         {
            rest_blocks = rest_blocks - cur_blocks * blocks_1;
            cur_blocks = rest_blocks;
            i = begin_2 + (cur_blocks * blockDim.x + threadIdx.x);
            if (i <= end_2) 
            {

// Loop body
               eps = max(eps, abs(b[i + b0002 * j] - a[i + a0002 * j]));
               a[i + a0002 * j] = b[i + b0002 * j];
            }
         }

// Reduction
         i = threadIdx.x + threadIdx.y * blockDim.x + threadIdx.z * (blockDim.x * blockDim.y);
         eps = __dvmh_blockReduceMax(eps);
         if (i % warpSize == 0) 
         {
            eps_grid[(add_blocks + blockIdx.x) * (blockDim.x * blockDim.y * blockDim.z / warpSize) + i / warpSize] = eps;
         }
      }


//--------------------- Kernel for loop on line 73 ---------------------

      __global__ void   loop_test2_73_cuda_kernel_int(float b[], __indexTypeInt b0002, float a[], __indexTypeInt a0002, __indexTypeInt begin_1, __indexTypeInt end_1, __indexTypeInt begin_2, __indexTypeInt end_2, __indexTypeInt blocks_1, __indexTypeInt add_blocks)
      {

// Local needs
         int i;
         int j;
         __indexTypeInt rest_blocks, cur_blocks;

// Calculate each thread's loop variables' values
         rest_blocks = add_blocks + blockIdx.x;
         cur_blocks = rest_blocks / blocks_1;
         j = begin_1 + (cur_blocks * blockDim.y + threadIdx.y);
         if (j <= end_1) 
         {
            rest_blocks = rest_blocks - cur_blocks * blocks_1;
            cur_blocks = rest_blocks;
            i = begin_2 + (cur_blocks * blockDim.x + threadIdx.x);
            if (i <= end_2) 
            {

// Loop body
               b[i + b0002 * j] = (a[i - 1 + a0002 * j] + a[i + a0002 * (j - 1)] + a[i + 1 + a0002 * j] + a[i + a0002 * (j + 1)]) / 4;
            }
         }
      }


//--------------------- Kernel for loop on line 73 ---------------------

      __global__ void   loop_test2_73_cuda_kernel_llong(float b[], __indexTypeLLong b0002, float a[], __indexTypeLLong a0002, __indexTypeLLong begin_1, __indexTypeLLong end_1, __indexTypeLLong begin_2, __indexTypeLLong end_2, __indexTypeLLong blocks_1, __indexTypeLLong add_blocks)
      {

// Local needs
         int i;
         int j;
         __indexTypeLLong rest_blocks, cur_blocks;

// Calculate each thread's loop variables' values
         rest_blocks = add_blocks + blockIdx.x;
         cur_blocks = rest_blocks / blocks_1;
         j = begin_1 + (cur_blocks * blockDim.y + threadIdx.y);
         if (j <= end_1) 
         {
            rest_blocks = rest_blocks - cur_blocks * blocks_1;
            cur_blocks = rest_blocks;
            i = begin_2 + (cur_blocks * blockDim.x + threadIdx.x);
            if (i <= end_2) 
            {

// Loop body
               b[i + b0002 * j] = (a[i - 1 + a0002 * j] + a[i + a0002 * (j - 1)] + a[i + 1 + a0002 * j] + a[i + a0002 * (j + 1)]) / 4;
            }
         }
      }



#ifdef _MS_F_
#define loop_test2_49_cuda_ loop_test2_49_cuda
#define loop_test2_65_cuda_ loop_test2_65_cuda
#define loop_test2_73_cuda_ loop_test2_73_cuda
#endif

extern "C" {
      extern  DvmType loop_test2_73_cuda_kernel_llong_regs, loop_test2_73_cuda_kernel_int_regs, loop_test2_65_cuda_kernel_llong_regs, loop_test2_65_cuda_kernel_int_regs, loop_test2_49_cuda_kernel_llong_regs, loop_test2_49_cuda_kernel_int_regs;


//    CUDA handler for loop on line 49 

      void   loop_test2_49_cuda_(DvmType *loop_ref, DvmType b[], DvmType a[], int *l)
      {
         void   *b_base, *a_base;
         DvmType d_b[5], d_a[5];
         DvmType idxTypeInKernel;
         dim3 blocks, threads;
         cudaStream_t stream;
         DvmType idxL[2], idxH[2];
         DvmType blocksS[2], restBlocks, maxBlocks, addBlocks, overallBlocks;
         DvmType device_num;

// Get device number
         device_num = loop_get_device_num_(loop_ref);

// Get 'natural' bases
         b_base = dvmh_get_natural_base(&device_num, b);
         a_base = dvmh_get_natural_base(&device_num, a);

// Fill 'device' headers
         dvmh_fill_header_(&device_num, b_base, b, d_b);
         dvmh_fill_header_(&device_num, a_base, a, d_a);

// Guess index type in CUDA kernel
         idxTypeInKernel = loop_guess_index_type_(loop_ref);
         if (idxTypeInKernel == rt_LONG && sizeof(long) == sizeof(int)) 
         {
            idxTypeInKernel = rt_INT;
         }
         if (idxTypeInKernel == rt_LONG && sizeof(long) == sizeof(long long)) 
         {
            idxTypeInKernel = rt_LLONG;
         }

// Get CUDA configuration parameters
         threads = dim3(0, 0, 0);
         if (idxTypeInKernel == rt_INT) 
         {
            loop_cuda_get_config(loop_ref, 0, loop_test2_49_cuda_kernel_int_regs, &threads, &stream, 0);
         }
         else 
         {
            loop_cuda_get_config(loop_ref, 0, loop_test2_49_cuda_kernel_llong_regs, &threads, &stream, 0);
         }
         loop_fill_bounds_(loop_ref, idxL, idxH, 0);
         blocksS[1] = (idxH[1] - idxL[1] + threads.x) / threads.x;
         blocksS[0] = blocksS[1] * ((idxH[0] - idxL[0] + threads.y) / threads.y);
         overallBlocks = blocksS[0];
         restBlocks = overallBlocks;
         addBlocks = 0;
         blocks = dim3(1, 1, 1);
         maxBlocks = loop_cuda_get_device_prop(loop_ref, CUDA_MAX_GRID_X);

// GPU execution
         while (restBlocks > 0)
         {
            if (restBlocks <= maxBlocks) 
            {
               blocks = restBlocks;
            }
            else 
            {
               blocks = maxBlocks;
            }
            if (idxTypeInKernel == rt_INT) 
            {
               loop_test2_49_cuda_kernel_int<<<blocks, threads, 0, stream>>>((float *)b_base, d_b[1], (float *)a_base, d_a[1], idxL[0], idxH[0], idxL[1], idxH[1], blocksS[1], addBlocks, *l);
            }
            else 
            {
               loop_test2_49_cuda_kernel_llong<<<blocks, threads, 0, stream>>>((float *)b_base, d_b[1], (float *)a_base, d_a[1], idxL[0], idxH[0], idxL[1], idxH[1], blocksS[1], addBlocks, *l);
            }
            addBlocks += blocks.x;
            restBlocks -= blocks.x;
         }
      }


//    CUDA handler for loop on line 65 

      void   loop_test2_65_cuda_(DvmType *loop_ref, DvmType a[], DvmType b[])
      {
         void   *a_base, *b_base;
         DvmType d_a[5], d_b[5];
         DvmType idxTypeInKernel;
         dim3 blocks, threads;
         cudaStream_t stream;
         DvmType idxL[2], idxH[2];
         DvmType blocksS[2], restBlocks, maxBlocks, addBlocks, overallBlocks;
         void   *eps_grid;
         float eps;
         DvmType red_num, num_of_red_blocks, fill_flag;
         DvmType shared_mem;
         DvmType device_num;

// Get device number
         device_num = loop_get_device_num_(loop_ref);

// Register reduction for CUDA-execution
         red_num = 1;
         loop_cuda_register_red(loop_ref, red_num, &eps_grid, 0);
         loop_red_init_(loop_ref, &red_num, &eps, 0);

// Get 'natural' bases
         a_base = dvmh_get_natural_base(&device_num, a);
         b_base = dvmh_get_natural_base(&device_num, b);

// Fill 'device' headers
         dvmh_fill_header_(&device_num, a_base, a, d_a);
         dvmh_fill_header_(&device_num, b_base, b, d_b);

// Guess index type in CUDA kernel
         idxTypeInKernel = loop_guess_index_type_(loop_ref);
         if (idxTypeInKernel == rt_LONG && sizeof(long) == sizeof(int)) 
         {
            idxTypeInKernel = rt_INT;
         }
         if (idxTypeInKernel == rt_LONG && sizeof(long) == sizeof(long long)) 
         {
            idxTypeInKernel = rt_LLONG;
         }

// Get CUDA configuration parameters
         threads = dim3(0, 0, 0);
#ifdef CUDA_FERMI_ARCH
         shared_mem = 4;
#else
         shared_mem = 0;
#endif
         if (idxTypeInKernel == rt_INT) 
         {
            loop_cuda_get_config(loop_ref, shared_mem, loop_test2_65_cuda_kernel_int_regs, &threads, &stream, &shared_mem);
         }
         else 
         {
            loop_cuda_get_config(loop_ref, shared_mem, loop_test2_65_cuda_kernel_llong_regs, &threads, &stream, &shared_mem);
         }
         loop_fill_bounds_(loop_ref, idxL, idxH, 0);
         blocksS[1] = (idxH[1] - idxL[1] + threads.x) / threads.x;
         blocksS[0] = blocksS[1] * ((idxH[0] - idxL[0] + threads.y) / threads.y);
         overallBlocks = blocksS[0];
         restBlocks = overallBlocks;
         addBlocks = 0;
         blocks = dim3(1, 1, 1);

// Prepare reduction
         num_of_red_blocks = overallBlocks * (threads.x * threads.y * threads.z / 32);
         fill_flag = 0;
         red_num = 1;
         loop_cuda_red_prepare(loop_ref, red_num, num_of_red_blocks, fill_flag);
         maxBlocks = loop_cuda_get_device_prop(loop_ref, CUDA_MAX_GRID_X);

// GPU execution
         while (restBlocks > 0)
         {
            if (restBlocks <= maxBlocks) 
            {
               blocks = restBlocks;
            }
            else 
            {
               blocks = maxBlocks;
            }
            if (idxTypeInKernel == rt_INT) 
            {
               loop_test2_65_cuda_kernel_int<<<blocks, threads, shared_mem, stream>>>((float *)a_base, d_a[1], (float *)b_base, d_b[1], eps, (float *)eps_grid, idxL[0], idxH[0], idxL[1], idxH[1], blocksS[1], addBlocks);
            }
            else 
            {
               loop_test2_65_cuda_kernel_llong<<<blocks, threads, shared_mem, stream>>>((float *)a_base, d_a[1], (float *)b_base, d_b[1], eps, (float *)eps_grid, idxL[0], idxH[0], idxL[1], idxH[1], blocksS[1], addBlocks);
            }
            addBlocks += blocks.x;
            restBlocks -= blocks.x;
         }

// Finish reduction
         red_num = 1;
         loop_red_finish(loop_ref, red_num);
      }


//    CUDA handler for loop on line 73 

      void   loop_test2_73_cuda_(DvmType *loop_ref, DvmType b[], DvmType a[])
      {
         void   *b_base, *a_base;
         DvmType d_b[5], d_a[5];
         DvmType idxTypeInKernel;
         dim3 blocks, threads;
         cudaStream_t stream;
         DvmType idxL[2], idxH[2];
         DvmType blocksS[2], restBlocks, maxBlocks, addBlocks, overallBlocks;
         DvmType device_num;

// Get device number
         device_num = loop_get_device_num_(loop_ref);

// Get 'natural' bases
         b_base = dvmh_get_natural_base(&device_num, b);
         a_base = dvmh_get_natural_base(&device_num, a);

// Fill 'device' headers
         dvmh_fill_header_(&device_num, b_base, b, d_b);
         dvmh_fill_header_(&device_num, a_base, a, d_a);

// Guess index type in CUDA kernel
         idxTypeInKernel = loop_guess_index_type_(loop_ref);
         if (idxTypeInKernel == rt_LONG && sizeof(long) == sizeof(int)) 
         {
            idxTypeInKernel = rt_INT;
         }
         if (idxTypeInKernel == rt_LONG && sizeof(long) == sizeof(long long)) 
         {
            idxTypeInKernel = rt_LLONG;
         }

// Get CUDA configuration parameters
         threads = dim3(0, 0, 0);
         if (idxTypeInKernel == rt_INT) 
         {
            loop_cuda_get_config(loop_ref, 0, loop_test2_73_cuda_kernel_int_regs, &threads, &stream, 0);
         }
         else 
         {
            loop_cuda_get_config(loop_ref, 0, loop_test2_73_cuda_kernel_llong_regs, &threads, &stream, 0);
         }
         loop_fill_bounds_(loop_ref, idxL, idxH, 0);
         blocksS[1] = (idxH[1] - idxL[1] + threads.x) / threads.x;
         blocksS[0] = blocksS[1] * ((idxH[0] - idxL[0] + threads.y) / threads.y);
         overallBlocks = blocksS[0];
         restBlocks = overallBlocks;
         addBlocks = 0;
         blocks = dim3(1, 1, 1);
         maxBlocks = loop_cuda_get_device_prop(loop_ref, CUDA_MAX_GRID_X);

// GPU execution
         while (restBlocks > 0)
         {
            if (restBlocks <= maxBlocks) 
            {
               blocks = restBlocks;
            }
            else 
            {
               blocks = maxBlocks;
            }
            if (idxTypeInKernel == rt_INT) 
            {
               loop_test2_73_cuda_kernel_int<<<blocks, threads, 0, stream>>>((float *)b_base, d_b[1], (float *)a_base, d_a[1], idxL[0], idxH[0], idxL[1], idxH[1], blocksS[1], addBlocks);
            }
            else 
            {
               loop_test2_73_cuda_kernel_llong<<<blocks, threads, 0, stream>>>((float *)b_base, d_b[1], (float *)a_base, d_a[1], idxL[0], idxH[0], idxL[1], idxH[1], blocksS[1], addBlocks);
            }
            addBlocks += blocks.x;
            restBlocks -= blocks.x;
         }
      }

}

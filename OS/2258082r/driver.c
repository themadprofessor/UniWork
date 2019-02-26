#include "driver.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h> // used for strerror

#ifndef OSX
#define CL_USE_DEPRECATED_OPENCL_1_1_APIS
#include <CL/cl.h>
#else
#include <OpenCL/opencl.h>
#endif

#define CL_ERR_STR(err) case err: return #err;

const char* clErrorString(int err) {
    switch (err) {
        CL_ERR_STR(CL_SUCCESS)
        CL_ERR_STR(CL_DEVICE_NOT_FOUND)
        CL_ERR_STR(CL_DEVICE_NOT_AVAILABLE)
        CL_ERR_STR(CL_COMPILER_NOT_AVAILABLE)
        CL_ERR_STR(CL_MEM_OBJECT_ALLOCATION_FAILURE)
        CL_ERR_STR(CL_OUT_OF_RESOURCES)
        CL_ERR_STR(CL_OUT_OF_HOST_MEMORY)
        CL_ERR_STR(CL_PROFILING_INFO_NOT_AVAILABLE)
        CL_ERR_STR(CL_MEM_COPY_OVERLAP)
        CL_ERR_STR(CL_IMAGE_FORMAT_MISMATCH)
        CL_ERR_STR(CL_IMAGE_FORMAT_NOT_SUPPORTED)
        CL_ERR_STR(CL_BUILD_PROGRAM_FAILURE)
        CL_ERR_STR(CL_MAP_FAILURE)
        CL_ERR_STR(CL_MISALIGNED_SUB_BUFFER_OFFSET)
        CL_ERR_STR(CL_EXEC_STATUS_ERROR_FOR_EVENTS_IN_WAIT_LIST)
        CL_ERR_STR(CL_COMPILE_PROGRAM_FAILURE)
        CL_ERR_STR(CL_LINKER_NOT_AVAILABLE)
        CL_ERR_STR(CL_LINK_PROGRAM_FAILURE)
        CL_ERR_STR(CL_DEVICE_PARTITION_FAILED)
        CL_ERR_STR(CL_KERNEL_ARG_INFO_NOT_AVAILABLE)
        CL_ERR_STR(CL_INVALID_VALUE)
        CL_ERR_STR(CL_INVALID_DEVICE_TYPE)
        CL_ERR_STR(CL_INVALID_PLATFORM)
        CL_ERR_STR(CL_INVALID_DEVICE)
        CL_ERR_STR(CL_INVALID_CONTEXT)
        CL_ERR_STR(CL_INVALID_QUEUE_PROPERTIES)
        CL_ERR_STR(CL_INVALID_COMMAND_QUEUE)
        CL_ERR_STR(CL_INVALID_HOST_PTR)
        CL_ERR_STR(CL_INVALID_MEM_OBJECT)
        CL_ERR_STR(CL_INVALID_IMAGE_FORMAT_DESCRIPTOR)
        CL_ERR_STR(CL_INVALID_IMAGE_SIZE)
        CL_ERR_STR(CL_INVALID_SAMPLER)
        CL_ERR_STR(CL_INVALID_BINARY)
        CL_ERR_STR(CL_INVALID_BUILD_OPTIONS)
        CL_ERR_STR(CL_INVALID_PROGRAM)
        CL_ERR_STR(CL_INVALID_PROGRAM_EXECUTABLE)
        CL_ERR_STR(CL_INVALID_KERNEL_NAME)
        CL_ERR_STR(CL_INVALID_KERNEL_DEFINITION)
        CL_ERR_STR(CL_INVALID_KERNEL)
        CL_ERR_STR(CL_INVALID_ARG_INDEX)
        CL_ERR_STR(CL_INVALID_ARG_VALUE)
        CL_ERR_STR(CL_INVALID_ARG_SIZE)
        CL_ERR_STR(CL_INVALID_KERNEL_ARGS)
        CL_ERR_STR(CL_INVALID_WORK_DIMENSION)
        CL_ERR_STR(CL_INVALID_WORK_GROUP_SIZE)
        CL_ERR_STR(CL_INVALID_WORK_ITEM_SIZE)
        CL_ERR_STR(CL_INVALID_GLOBAL_OFFSET)
        CL_ERR_STR(CL_INVALID_EVENT_WAIT_LIST)
        CL_ERR_STR(CL_INVALID_EVENT)
        CL_ERR_STR(CL_INVALID_OPERATION)
        CL_ERR_STR(CL_INVALID_GL_OBJECT)
        CL_ERR_STR(CL_INVALID_BUFFER_SIZE)
        CL_ERR_STR(CL_INVALID_MIP_LEVEL)
        CL_ERR_STR(CL_INVALID_GLOBAL_WORK_SIZE)
        CL_ERR_STR(CL_INVALID_PROPERTY)
        CL_ERR_STR(CL_INVALID_IMAGE_DESCRIPTOR)
        CL_ERR_STR(CL_INVALID_COMPILER_OPTIONS)
        CL_ERR_STR(CL_INVALID_LINKER_OPTIONS)
        CL_ERR_STR(CL_INVALID_DEVICE_PARTITION_COUNT)
        CL_ERR_STR(CL_INVALID_PIPE_SIZE)
        CL_ERR_STR(CL_INVALID_DEVICE_QUEUE)
        //CL_ERR_STR(CL_INVALID_SPEC_ID)
        //CL_ERR_STR(CL_MAX_SIZE_RESTRICTION_EXCEEDED)
        default:
            return "Unknown OpenCL Error";
    }
}

////////////////////////////////////////////////////////////////////////////////
CLObject* init_driver() {
    CLObject* ocl = (CLObject*)malloc(sizeof(CLObject));
    int err;                            // error code returned from api calls

    unsigned int status[1]={0};               // number of correct results returned

    size_t global;                      // global domain size for our calculation
    size_t local;                       // local domain size for our calculation

    cl_device_id device_id;             // compute device id 
    cl_context context;                 // compute context
    cl_command_queue command_queue;          // compute command queue
    cl_program program;                 // compute program
    cl_kernel kernel;                   // compute kernel

    cl_mem input1, input2;                       // device memory used for the input array
    cl_mem output, status_buf;                      // device memory used for the output array

    FILE* programHandle;
    size_t programSize;
    char *programBuffer;

    cl_uint nplatforms;
    err = clGetPlatformIDs(0, NULL, &nplatforms);
    if (err != CL_SUCCESS) {
        fprintf(stderr,"Error: Failed to get number of platform: %d!\n", err);
        exit(EXIT_FAILURE);

    }

    // Now ask OpenCL for the platform IDs:
    cl_platform_id* platforms = (cl_platform_id*)malloc(sizeof(cl_platform_id) * nplatforms);
    err = clGetPlatformIDs(nplatforms, platforms, NULL);
    if (err != CL_SUCCESS) {
        fprintf(stderr,"Error: Failed to get platform IDs: %d!\n",err);
        exit(EXIT_FAILURE);

    }
#ifdef GPU
    err = clGetDeviceIDs(platforms[0], CL_DEVICE_TYPE_GPU, 1, &device_id, NULL);
#else
    err = clGetDeviceIDs(platforms[0], CL_DEVICE_TYPE_CPU, 1, &device_id, NULL);
#endif
    if (err != CL_SUCCESS)
    {
        fprintf(stderr,"Error: Failed to create a device group: %d!\n",err);
        exit(EXIT_FAILURE);

    }

    // Create a compute context 
    //
    context = clCreateContext(0, 1, &device_id, NULL, NULL, &err);
    if (!context)
    {
        fprintf(stderr,"Error: Failed to create a compute context: %d!\n",err);
        exit(EXIT_FAILURE);

    }

    // Create a command command_queue
    //
    command_queue = clCreateCommandQueue(context, device_id, 0, &err);
    if (!command_queue)
    {
        fprintf(stderr,"Error: Failed to create a command command_queue: %d!\n",err);
        exit(EXIT_FAILURE);

    }
    // get size of kernel source
    programHandle = fopen("./firmware.cl", "r");
    fseek(programHandle, 0, SEEK_END);
    programSize = ftell(programHandle);
    rewind(programHandle);

    // read kernel source into buffer
    programBuffer = (char*) malloc(programSize + 1);
    programBuffer[programSize] = '\0';
    fread(programBuffer, sizeof(char), programSize, programHandle);
    fclose(programHandle);

    // create program from buffer
    program = clCreateProgramWithSource(context, 1, (const char**) &programBuffer, &programSize, &err);
    free(programBuffer);
    if (!program)
    {
        fprintf(stderr,"Error: Failed to create compute program: %d!\n",err);
        exit(EXIT_FAILURE);

    }

    // Build the program executable
    //
    err = clBuildProgram(program, 0, NULL, NULL, NULL, NULL);
    if (err != CL_SUCCESS)
    {
        size_t len;
        char buffer[2048];

        fprintf(stderr,"Error: Failed to build program executable: %d!\n",err);
        clGetProgramBuildInfo(program, device_id, CL_PROGRAM_BUILD_LOG, sizeof(buffer), buffer, &len);
        fprintf(stderr,"%s\n", buffer);
        exit(EXIT_FAILURE);
    }

    // Create the compute kernel in the program we wish to run
    //
    kernel = clCreateKernel(program, "firmware", &err);
    if (!kernel || err != CL_SUCCESS)
    {
        fprintf(stderr,"Error: Failed to create compute kernel: %d!\n",err);
        exit(EXIT_FAILURE);

    }
    ocl->context = context;
    ocl->command_queue = command_queue;
    ocl->kernel = kernel;
    ocl->program= program;
    ocl->device_id = device_id;

//===============================================================================================================================================================  
// START of assignment code section 
//    [YOUR CODE HERE]
    err = pthread_mutex_init(&ocl->device_lock, NULL);
    if (err != 0) {
        fprintf(stderr, "Error: Failed to init mutex: %s\n", strerror(err));
        exit(EXIT_FAILURE);
    }
// END of assignment code section 
//===============================================================================================================================================================  

    return ocl;
}

int shutdown_driver(CLObject* ocl) {
    int err = clReleaseProgram(ocl->program);
    if (err != CL_SUCCESS) {
        fprintf(stderr,"Error: Failed to release Program: %d!\n",err);
        exit(EXIT_FAILURE);
    }
    err = clReleaseKernel(ocl->kernel);
    if (err != CL_SUCCESS) {
        fprintf(stderr,"Error: Failed to release Kernel: %d!\n",err);
        exit(EXIT_FAILURE);
    }
    err = clReleaseCommandQueue(ocl->command_queue);
    if (err != CL_SUCCESS) {
        fprintf(stderr,"Error: Failed to release Command Queue: %d!\n",err);
        exit(EXIT_FAILURE);
    }
    err = clReleaseContext(ocl->context);
    if (err != CL_SUCCESS) {
        fprintf(stderr,"Error: Failed to release Context: %d!\n",err);
        exit(EXIT_FAILURE);
    }
//===============================================================================================================================================================  
// START of assignment code section
    err = pthread_mutex_destroy(&ocl->device_lock);
    if (err != 0) {
        fprintf(stderr, "Error: Failed to destroy mutex: %s\n", strerror(err));
    }
// END of assignment code section
//===============================================================================================================================================================  

    free(ocl);
    return 0;
}

////////////////////////////////////////////////////////////////////////////////

int run_driver(CLObject* ocl,unsigned int buffer_size,  int* input_buffer_1, int* input_buffer_2, int w1, int w2, int* output_buffer) {
    long long unsigned int tid = ocl->thread_num;
#if VERBOSE_MT>2
    printf("run_driver thread: %llu\n",tid);
#endif
    int err;                            // error code returned from api calls
    int status[1]={-1};               // number of correct results returned
    unsigned int max_iters;
    max_iters = MAX_ITERS;

    size_t global;                      // global domain size for our calculation
    size_t local;                       // local domain size for our calculation

    cl_mem input1, input2;          // device memory used for the input array
    cl_mem output, status_buf;                      // device memory used for the output array
    err = pthread_mutex_lock(&ocl->device_lock);
    if (err != 0) {
        fprintf(stderr, "Error: Failed to lock mutex: %s\n", strerror(err));
        exit(EXIT_FAILURE);
    }

    // Get the maximum work group size for executing the kernel on the device
    err = clGetKernelWorkGroupInfo(ocl->kernel, ocl->device_id, CL_KERNEL_WORK_GROUP_SIZE, sizeof(local), &local, NULL);
    if (err != CL_SUCCESS) {
        fprintf(stderr,"Error: Failed to retrieve kernel work group info! %d\n", err);
        exit(EXIT_FAILURE);
    }

    global = buffer_size; // create as meany threads on the device as there are elements in the array

//===============================================================================================================================================================  
// START of assignment code section 

    // You must make sure the driver is thread-safe by using the appropriate POSIX mutex operations
    // You must also check the return value of every API call and handle any errors 

    // Create the buffer objects to link the input and output arrays in device memory to the buffers in host memory

    cl_mem_flags in_flags = CL_MEM_READ_ONLY | CL_MEM_HOST_WRITE_ONLY;
    cl_mem_flags out_flags = CL_MEM_WRITE_ONLY | CL_MEM_HOST_READ_ONLY;
    int iter_count = 0;

    input1 = clCreateBuffer(ocl->context, in_flags, sizeof(int) * buffer_size, NULL, &err);
    if (input1 == NULL) {
        fprintf(stderr, "Error: Failed to allocate input1! %s\n", clErrorString(err));
        exit(EXIT_FAILURE);
    }

    input2 = clCreateBuffer(ocl->context, in_flags, sizeof(int) * buffer_size, NULL, &err);
    if (input2 == NULL) {
        fprintf(stderr, "Error: Failed to allocate input2! %s\n", clErrorString(err));
        exit(EXIT_FAILURE);
    }

    status_buf = clCreateBuffer(ocl->context, out_flags, sizeof(status[0]), NULL, &err);
    if (status_buf == NULL) {
        fprintf(stderr, "Error: Failed to allocate status_buf! %s\n", clErrorString(err));
        exit(EXIT_FAILURE);
    }

    output = clCreateBuffer(ocl->context, out_flags, sizeof(int) * buffer_size, NULL, &err);
    if (output == NULL) {
        fprintf(stderr, "Error: Failed to allocate output! %s\n", clErrorString(err));
        exit(EXIT_FAILURE);
    }

    // Write the data in input arrays into the device memory
    do {
        err = clEnqueueWriteBuffer(ocl->command_queue, input1, CL_TRUE, 0, sizeof(int) * buffer_size, input_buffer_1, 0,
                                   NULL, NULL);
        if (err != CL_SUCCESS) {
            fprintf(stderr, "Error: Failed to copy data to input1! %s\n", clErrorString(err));
            exit(EXIT_FAILURE);
        }

        err = clEnqueueWriteBuffer(ocl->command_queue, input2, CL_TRUE, 0, sizeof(int) * buffer_size, input_buffer_2, 0,
                                   NULL, NULL);
        if (err != CL_SUCCESS) {
            fprintf(stderr, "Error: Failed to copy data to input2! %s\n", clErrorString(err));
            exit(EXIT_FAILURE);
        }

        // Set the arguments to our compute kernel

        err = clSetKernelArg(ocl->kernel, 0, sizeof(&input1), &input1);
        if (err != CL_SUCCESS) {
            fprintf(stderr, "Error: Failed to set kernel arg0! %s\n", clErrorString(err));
            exit(EXIT_FAILURE);
        }

        err = clSetKernelArg(ocl->kernel, 1, sizeof(&input2), &input2);
        if (err != CL_SUCCESS) {
            fprintf(stderr, "Error: Failed to set kernel arg1! %s\n", clErrorString(err));
            exit(EXIT_FAILURE);
        }

        err = clSetKernelArg(ocl->kernel, 2, sizeof(&output), &output);
        if (err != CL_SUCCESS) {
            fprintf(stderr, "Error: Failed to set kernel arg2! %s\n", clErrorString(err));
            exit(EXIT_FAILURE);
        }

        err = clSetKernelArg(ocl->kernel, 3, sizeof(&status_buf), &status_buf);
        if (err != CL_SUCCESS) {
            fprintf(stderr, "Error: Failed to set kernel arg3! %s\n", clErrorString(err));
            exit(EXIT_FAILURE);
        }

        err = clSetKernelArg(ocl->kernel, 4, sizeof(w1), &w1);
        if (err != CL_SUCCESS) {
            fprintf(stderr, "Error: Failed to set kernel arg4! %s\n", clErrorString(err));
            exit(EXIT_FAILURE);
        }

        err = clSetKernelArg(ocl->kernel, 5, sizeof(w2), &w2);
        if (err != CL_SUCCESS) {
            fprintf(stderr, "Error: Failed to set kernel arg5! %s\n", clErrorString(err));
            exit(EXIT_FAILURE);
        }

        err = clSetKernelArg(ocl->kernel, 6, sizeof(buffer_size), &buffer_size);
        if (err != CL_SUCCESS) {
            fprintf(stderr, "Error: Failed to set kernel arg6! %s\n", clErrorString(err));
            exit(EXIT_FAILURE);
        }

        // Execute the kernel, i.e. tell the device to process the data using the given global and local ranges

#if VERBOSE_MT > 0
        printf("local: %lu\n", local);
        printf("global: %lu\n", global);
#endif
        cl_event kernel_event;
        err = clEnqueueNDRangeKernel(ocl->command_queue, ocl->kernel, (cl_uint) 1, NULL, &global, NULL, 0, NULL,
                                     &kernel_event);
        if (err != CL_SUCCESS) {
            fprintf(stderr, "Error: Failed to enqueue the kernel! %s\n", clErrorString(err));
            exit(EXIT_FAILURE);
        }

        // Wait for the command commands to get serviced before reading back results. This is the device sending an interrupt to the host

        // Reading the status buffer waits for kernel_event to be complete before reading

        // Check the status

        err = clEnqueueReadBuffer(ocl->command_queue, status_buf, CL_TRUE, 0, sizeof(status), status, 1, &kernel_event,
                                  NULL);
        if (err != CL_SUCCESS) {
            fprintf(stderr, "Error: Failed to enqueue status read! %s\n", clErrorString(err));
            exit(EXIT_FAILURE);
        }

        // When the status is 0, read back the results from the device to verify the output

    } while (status[0] != 0 && ++iter_count < max_iters);

    if (status[0] != 0) {
        fprintf(stderr, "You done goofed!\n");
        exit(EXIT_FAILURE);
    }

    err = clEnqueueReadBuffer(ocl->command_queue, output, CL_TRUE, 0, sizeof(int) * buffer_size, output_buffer, 0, NULL, NULL);
    if (err != CL_SUCCESS) {
        fprintf(stderr, "Error: Failed to read output! %s\n", clErrorString(err));
        exit(EXIT_FAILURE);
    }

    // Shutdown and cleanup

    err = clReleaseMemObject(input1);
    if (err != CL_SUCCESS) {
        fprintf(stderr, "Error: Failed to release buffer! %s\n", clErrorString(err));
    }

    err = clReleaseMemObject(input2);
    if (err != CL_SUCCESS) {
        fprintf(stderr, "Error: Failed to release buffer! %s\n", clErrorString(err));
    }

    err = clReleaseMemObject(output);
    if (err != CL_SUCCESS) {
        fprintf(stderr, "Error: Failed to release buffer! %s\n", clErrorString(err));
    }

    err = clReleaseMemObject(status_buf);
    if (err != CL_SUCCESS) {
        fprintf(stderr, "Error: Failed to release buffer! %s\n", clErrorString(err));
    }

    err = pthread_mutex_unlock(&ocl->device_lock);
    if (err != 0) {
        fprintf(stderr, "Error: Failed to unlock mutex! %s\n", strerror(err));
        exit(EXIT_FAILURE);
    }

// END of assignment code section 
//===============================================================================================================================================================  
    return *status;

}

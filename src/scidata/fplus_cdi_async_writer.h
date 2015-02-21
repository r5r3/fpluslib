#ifndef FPLUS_CDI_ASYNC_WRITER_H
#define FPLUS_CDI_ASYNC_WRITER_H

#include <iostream>
#include <thread>
#include <mutex>
#include <deque>
#include <map>
#include <atomic>
#include <string.h>
#include <unistd.h>

using namespace std;

// structure to store information for the definition of new time steps or data for new time steps
#define OTYPE_streamWriteVar    1
#define OTYPE_streamDefTimestep 2

// maximal size of the buffer (currently 100 MB), !! currently unused !!
#define CDI_ASYNC_MAX_BUFFER    104857600

struct async_operation {
    ~async_operation();
    int oper_type;
    int streamID;
    int varID;
    double* values = nullptr;
    int nmiss;
    int nvalues;
    int taxisID;
    int idate;
    int itime;
    int ind;
};

class cdi_async_writer {
public:
    cdi_async_writer();
    ~cdi_async_writer();
    void add_operation(async_operation* op);
private:
    void write_loop();
    atomic<size_t> size;
    atomic<bool> write_all_and_exit;
    mutex* mtx;
    deque<async_operation*> queue;
    thread* write_thread;
};

// interface to c api
#ifdef __cplusplus
extern "C" {
#endif

    #include <cdi.h>
    int init_writer();
    void destroy_writer(int index);
    void streamWriteVar_async(int writer, int streamID, int varID, double* values, int nmiss, int nvalues);
    void streamDefTimestep_async(int writer, int taxisID, int idate, int itime, int streamID, int ind);

#ifdef __cplusplus
}
#endif

#endif
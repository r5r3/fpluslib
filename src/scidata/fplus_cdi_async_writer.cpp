#include "fplus_cdi_async_writer.h"

// constructor for the writer
cdi_async_writer::cdi_async_writer() {
    this->mtx = new mutex();
    this->size = 0;
    this->write_thread = new thread(&cdi_async_writer::write_loop, this);
}

// destructor for the writer
cdi_async_writer::~cdi_async_writer() {
    // write out all data to disk
    this->write_all_and_exit = true;
    this->write_thread->join();
    // clean up
    delete this->mtx;
    delete this->write_thread;
}

// place a new operation in the queue
void cdi_async_writer::add_operation(async_operation &op) {
    // lock for other operations
    this->mtx->lock();

    // check the operation type
    switch (op.oper_type) {

        // write data for one time step
        case OTYPE_streamWriteVar:
            this->size += op.nvalues*sizeof(double);
            break;

        // define a new time step
        case OTYPE_streamDefTimestep:
            break;

        // unsupported operation!
        default:
            cerr << "ERROR: unsupported operation in asynchronous writer: " << op.oper_type << endl;
            exit(-1);
    }

    // add to queue and unlock
    this->queue.push_back(op);
    this->mtx->unlock();
}

// endless loop which is used to write data to disk
void cdi_async_writer::write_loop() {
    int status;
    while (true) {
        // wait for new data
        if (!this->write_all_and_exit || this->size == 0) usleep(100000);

        // get a new operation from the writer, first step: lock it
        this->mtx->lock();
        // no elements, cycle! 
        if (this->queue.size() == 0) {
            this->mtx->unlock();
            // leave the loop if this is the final write process
            if (this->write_all_and_exit) {
                break;
            } else {
                continue;                
            }
        }
        async_operation op = this->queue.front();
        this->queue.pop_front();
        this->mtx->unlock();

        // perform the operation
        switch (op.oper_type) {

            // write data for one time step
            case OTYPE_streamWriteVar:
                streamWriteVar(op.streamID, op.varID, op.values, op.nmiss);
                //cleanup 
                delete op.values;
                //reduce size
                this->mtx->lock();
                this->size -= op.nvalues*sizeof(double);
                this->mtx->unlock();
                break;

            // define a new time step
            case OTYPE_streamDefTimestep:
                taxisDefVdate(op.taxisID, op.idate);
                taxisDefVtime(op.taxisID, op.itime);
                status = streamDefTimestep(op.streamID, op.ind);
                break;
        }
    }
}

// all writer objects are stored in a map. The fortran routines get only the index returned 
map<int,cdi_async_writer*> writers;


// create the writer object
int init_writer() {
    int index = writers.size()+1;
    cdi_async_writer* writer = new cdi_async_writer();
    writers[index] = writer;
    return index;
}

// write all data to disk and free the memory
void destroy_writer(int index) {
    cdi_async_writer* writer = writers[index];
    delete writer;
    writers.erase(index);
}

void streamWriteVar_async(int writer, int streamID, int varID, double* values, int nmiss, int nvalues) {
    // copy all information about this operation
    async_operation op;
    op.oper_type = OTYPE_streamWriteVar;
    op.streamID = streamID;
    op.varID = varID;
    op.values = new double[nvalues];
    memcpy(op.values, values, nvalues*sizeof(double));
    op.nvalues = nvalues;
    op.nmiss = nmiss;
    // put this operation into the queue
    writers[writer]->add_operation(op);
}

void streamDefTimestep_async(int writer, int taxisID, int idate, int itime, int streamID, int ind) {
    // copy all information about this operation
    async_operation op;
    op.oper_type = OTYPE_streamDefTimestep;
    op.taxisID = taxisID;
    op.idate = idate;
    op.itime = itime;
    op.streamID = streamID;
    op.ind = ind;
    // put this operation into the queue
    writers[writer]->add_operation(op);
}
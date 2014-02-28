/**
 * @file
 * @author Sergey Smirnov <sasmir@gmail.com>
 */

#include "ErlPortInterface.h"

#include <pthread.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <stdlib.h>

typedef unsigned char byte;

int read_exact(byte *buf, int len);
int write_exact(byte *buf, int len);
int read_cmd(byte *buf);
int write_cmd(byte *buf, int len);

bool isBetter(double oldVal, double newVal)
{
    return newVal < oldVal && oldVal - newVal > 0.000001;
}

void writeDouble(byte *buf, double val)
{
    unsigned long long *p = (unsigned long long *)&val;
    for (size_t i = 0; i < 8; ++i)
    {
        buf[7 - i] = (*p) & 0xFF;
        *p >>= 8;
    }
}

double readDouble(byte *buf)
{
    unsigned long long l = buf[0];
    for (size_t i = 1; i < 8; ++i)
    {
        l <<= 8;
        l += (unsigned long long)buf[i];
    }
    return *((double *)&l);
}

ErlPortInterface::ErlPortInterface()
{
    _state = BV_NONE;
    _bEnabled = false;
    pthread_mutex_init(&_mutex, NULL);
}

void ErlPortInterface::writeResult(const std::string &status, double bestValue)
{
    char buf[100];
    buf[0] = 2;
    char *p = buf + 1;
    writeDouble((byte *)p, bestValue);
    p += 8;

    sprintf(p, "%s", status.c_str());
    
    //fprintf(stderr, ">>> sendResult: %lf, %s\n", result, p);
    if (_bEnabled)
    {
        write_cmd((byte *)buf, p - buf + strlen(p));
    }
}

int read_cmd(byte *buf)
{
    int len;

    if (read_exact(buf, 2) != 2)
        return(-1);
    len = (buf[0] << 8) | buf[1];
    return read_exact(buf, len);
}

int write_cmd(byte *buf, int len)
{
    byte li;

    li = (len >> 8) & 0xff;
    write_exact(&li, 1);
  
    li = len & 0xff;
    write_exact(&li, 1);

    return write_exact(buf, len);
}

int read_exact(byte *buf, int len)
{
    int i, got=0;

    do {
        if ((i = read(3, buf+got, len-got)) <= 0)
        {
            return(i);
        }
        got += i;
    } while (got<len);

    return(len);
}

int write_exact(byte *buf, int len)
{
    int i, wrote = 0;

    do {
        if ((i = write(4, buf+wrote, len-wrote)) <= 0)
            return (i);
        wrote += i;
    } while (wrote<len);

    return (len);
}

void *ErlPortInterface::readerLoop(void *ptr)
{
    ErlPortInterface *This = (ErlPortInterface *)ptr;

    byte buf[100];
    int len = 0;
    while ((len = read_cmd(buf)) > 0) {
        switch (buf[0])
        {
        case 1:
            if (len != 9)
            {
                fprintf(stderr, "Wrong input message size: %d != 9\n", len);
                exit(1);
            }
            fprintf(stderr, ">>> readerLoop(): received best solution: %lf\n",
                readDouble(buf + 1));
            This->setBestValue(readDouble(buf + 1), false);
            break;
        }
    }
    fprintf(stderr, "read_cmd() failed: %d\n", len);
    exit(2);
}

void ErlPortInterface::sendIncumbent(double value)
{
    fprintf(stderr, ">>> sendIncumbent(): %lf\n", value);
    if (_bEnabled)
    {
        byte buf[1 + 8];
        buf[0] = 3;
        writeDouble(buf + 1, value);
        write_cmd(buf, sizeof(buf));
    }
}

void ErlPortInterface::setBestValue(double value, bool fromSolver)
{
    pthread_mutex_lock(&_mutex);
    switch (_state)
    {
    case BV_NONE:
        _state = fromSolver ? BV_FROM_SOLVER : BV_FROM_ERL;
        _bestValue = value;
        if (fromSolver)
        {
            sendIncumbent(value);
        }
        break;
    default:
        if (isBetter(_bestValue, value))
        {
            _state = fromSolver ? BV_FROM_SOLVER : BV_FROM_ERL;
            _bestValue = value;
            if (fromSolver)
            {
                sendIncumbent(value);
            }
        }
    }
    pthread_mutex_unlock(&_mutex);
}

void ErlPortInterface::getBestValue(BestValueAcceptor &acceptor)
{
    pthread_mutex_lock(&_mutex);
    if (_state == BV_FROM_ERL)
    {
        fprintf(stderr, ">>> getBestValue(): setting best solution in solver: %lf\n", _bestValue);
        acceptor.acceptNewBestValue(_bestValue);
        _state = BV_FROM_SOLVER;
    }
    pthread_mutex_unlock(&_mutex);
}


void ErlPortInterface::initialize(bool enabled, double bestVal)
{
    _state = BV_FROM_SOLVER;
    _bestValue = bestVal;
    initialize(enabled);
}

void ErlPortInterface::initialize(bool enabled)
{
    _bEnabled = enabled;

    if (_bEnabled)
    {
        pthread_t thread;
        int ret;
        if ((ret = pthread_create(&thread, NULL, readerLoop, this)))
        {
            fprintf(stderr, ">>> pthread_create() failed with %d\n", ret);
            exit(1);
        }
    }
}

BestValueAcceptor::~BestValueAcceptor()
{
}

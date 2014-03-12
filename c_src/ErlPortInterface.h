/**
 * @file
 * @author Sergey Smirnov <sasmir@gmail.com>
 */

#ifndef __ERLPORTINTERFACE_H__
#define __ERLPORTINTERFACE_H__

#include <string>
#include <pthread.h>

class BestValueAcceptor {
 public:
    virtual void acceptNewBestValue(double bestVal) = 0;
    virtual ~BestValueAcceptor() = 0;
};

class ErlPortInterface
{
 public:
    ErlPortInterface();

    void initialize(bool enabled, double bestValue);

    void initialize(bool enabled);

    void setBestValue(double value, bool fromSolver);

    void getBestValue(BestValueAcceptor &acceptor);

    void writeResult(const std::string &status, double bestValue);

    void setQuiet(bool quiet);

 private:
    enum BestValueState
    {
        BV_NONE,
        BV_FROM_SOLVER,
        BV_FROM_ERL
    };

    BestValueState _state;
    double _bestValue;
    bool _bEnabled;
    pthread_mutex_t _mutex;
    bool _quiet;

    static void *readerLoop(void *);
    void sendIncumbent(double value);

};

#endif // __ERLPORTINTERFACE_H__

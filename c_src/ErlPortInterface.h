/**
 * @file
 * @author Sergey Smirnov <sasmir@gmail.com>
 */

#ifndef __ERLPORTINTERFACE_H__
#define __ERLPORTINTERFACE_H__

#include <string>
#include <pthread.h>
#include <cstdint>

bool isBetter(double oldVal, double newVal);

class BestValueAcceptor {
 public:
    virtual void acceptNewBestValue(double bestVal, std::uint16_t seqNumber) = 0;
    virtual ~BestValueAcceptor() = 0;
};

class ErlPortInterface
{
 public:
    ErlPortInterface();

    void initialize(bool enabled, double bestValue, BestValueAcceptor *acceptor = NULL);

    void initialize(bool enabled, BestValueAcceptor *acceptor = NULL);

    bool setBestValue(double value, bool fromSolver, std::uint16_t seqNumber = 0);

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
    std::uint16_t _bestValueSeqNumber;
    bool _bEnabled;
    pthread_mutex_t _mutex;
    bool _quiet;
    BestValueAcceptor *_acceptor;

    static void *readerLoop(void *);
    void sendIncumbent(double value, std::uint16_t seqNumber);

};

#endif // __ERLPORTINTERFACE_H__

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

#include "CbcModel.hpp"
#include "OsiClpSolverInterface.hpp"
#include "CbcCompareDefault.hpp"

int g_stdoutFd = 1;

ErlPortInterface g_portInterface;

void sendResult(CbcModel &model, double result)
{
    if (model.isProvenOptimal())
    {
        g_portInterface.writeResult("optimal", result);
        return;
    }

    if (model.isProvenInfeasible())
    {
        g_portInterface.writeResult("infeasible", result);
        return;
    }

    g_portInterface.writeResult("stopped", result);
}

class MyCbcCompare : public CbcCompareBase, public BestValueAcceptor
{
public:
    MyCbcCompare(CbcModel *model)
    {
        _model = model;
    }
    bool test(CbcNode *x, CbcNode *y)
    {
        g_portInterface.getBestValue(*this);
        return _cmp.test(x, y);
    }
    
    bool newSolution(CbcModel *model, double objectiveAtContinuous,
        int numberInfeasibilitiesAtContinuous)
    {
        _model = model;
        g_portInterface.getBestValue(*this);
        return _cmp.newSolution(model, objectiveAtContinuous,
            numberInfeasibilitiesAtContinuous);
    }

    bool every1000Nodes(CbcModel *model, int numberNodes)
    {
        _model = model;
        g_portInterface.getBestValue(*this);
        return _cmp.every1000Nodes(model, numberNodes);
    }

    CbcCompareBase *clone() const
    {
        MyCbcCompare *result = new MyCbcCompare(*this);
        result->_model = _model;
        return result;
    }

    void acceptNewBestValue(double bestVal)
    {
        _model->setBestSolution(NULL, 0, bestVal);
    }

private:
    CbcCompareDefault _cmp;
    CbcModel *_model;
};

void processLine(const char *line)
{
    const char *solString = "Integer solution of";
    const char *p = strstr(line, solString);
    if (p != NULL)
    {
        const char *incumbentValue = p + strlen(solString);
        g_portInterface.setBestValue(atof(incumbentValue), true);
    }
}

void *pipeReaderLoop(void *arg)
{
    char buf[1024];
    int tail = 0;
    do
    {
        int ret = read((int)(long)arg, buf + tail, sizeof(buf) - tail - 1);
        if (ret <= 0)
        {
            fprintf(stderr, "read() from pipe failed: %s\n", strerror(errno));
            exit(1);
        }
        
        write(g_stdoutFd, buf, ret);
        
        int start = 0;
        for (int i = 0; i < ret; ++i)
        {
            if (buf[i] == '\n')
            {
                buf[i] = '\0';
                processLine(buf + start);
                start = i + 1;
            }
        }
        int tail = ret - start;
        if (tail > 0)
        {
            memmove(buf, buf + start, tail);
        }
    }
    while (true);
}

int main(int argc, char **argv)
{
    if (argc < 2)
    {
        fprintf(stderr, "Usage: %s <AMPL stub path> [-p] [-o <log file>] [-b <best solution value>] [-- CBC args]\n", argv[0]);
        return 1;
    }

    OsiClpSolverInterface solver;
    CbcModel model(solver);

    const char *logFileName = NULL;

    bool usePort = false;
    bool haveInitialBestVal = false;
    double initialBestVal = 0;

    char **p = argv;
    for (; *p && strcmp(*p, "--"); ++p)
    {
        if (!strcmp(*p, "-b"))
        {
            haveInitialBestVal = true;
            initialBestVal = atof(*(p + 1));
            model.setBestSolution(NULL, 0, initialBestVal);
            ++p;
        }
        if (!strcmp(*p, "-p"))
        {
            usePort = true;
        }
        if (!strcmp(*p, "-q"))
        {
            g_portInterface.setQuiet(true);
        }
        if (!strcmp(*p, "-o"))
        {
            logFileName = *(p + 1);
            ++p;
        }
    }

    MyCbcCompare cmp(&model);
    model.setNodeComparison(cmp);

    if (haveInitialBestVal)
    {
        g_portInterface.initialize(usePort, initialBestVal);
    }
    else
    {
        g_portInterface.initialize(usePort);
    }

    int pipefds[2];
    if (pipe(pipefds))
    {
        fprintf(stderr, "pipe() failed: %s\n", strerror(errno));
        return 1;
    }
    g_stdoutFd = dup(1);
    dup2(pipefds[1], 1);
    pthread_t pipeReaderThread;
    if (pthread_create(&pipeReaderThread, NULL, pipeReaderLoop, (void *)(long)pipefds[0]))
    {
        return 1;
    }

    FILE *f = NULL;
    int oldStdout, oldStderr;
    if (logFileName != NULL)
    {
        f = fopen(logFileName, "w");
        if (f == NULL)
        {
            fprintf(stderr, "Failed to open %s for writing\n", logFileName);
            return 1;
        }
        oldStdout = dup(g_stdoutFd);
        dup2(fileno(f), g_stdoutFd);
        oldStderr = dup(2);
        dup2(fileno(f), 2);
    }

    std::vector<std::string> rawArgs;
    rawArgs.push_back("cbc");
    rawArgs.push_back(argv[1]);
    rawArgs.push_back("-AMPL");
    rawArgs.push_back("wantsol=1");
    rawArgs.push_back("log=1");
    if (*p)
    {
        for (++p; *p; ++p)
        {
            rawArgs.push_back(*p);
        }
    }

    std::vector<const char *> args;
    for (size_t i = 0; i < rawArgs.size(); ++i)
    {
        args.push_back(rawArgs[i].c_str());
    }
    args.push_back(NULL);

    int res = CbcMain(rawArgs.size(), &(args[0]), model);

    fflush(stdout);
    fflush(stderr);

    fprintf(stderr, ">>> CbcMain: %d %d %d\n", res, model.status(), model.secondaryStatus());

    sendResult(model, model.getObjValue());

    fflush(stderr);

    return 0;
}

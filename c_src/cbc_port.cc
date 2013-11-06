#include <pthread.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <fcntl.h>
#include <errno.h>

#include "CbcModel.hpp"
#include "OsiClpSolverInterface.hpp"
#include "CbcCompareDefault.hpp"

typedef unsigned char byte;

static pthread_mutex_t g_mutex = PTHREAD_MUTEX_INITIALIZER;

int g_stdoutFd = 1;
bool g_bPortMode = false;

enum IncumbentState
{
    IS_NONE,
    IS_FROM_CBC,
    IS_FROM_ERL
};

IncumbentState g_incumbentState;
double g_incumbentValue;

void haveNewIncumbent(double value, bool fromCbc);
void updateIncumbentInCBC(CbcModel *model);

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

void sendResult(int status, int status2, double result)
{
    char buf[100];
    buf[0] = 2;
    char *p = buf + 1;
    writeDouble((byte *)p, result);
    p += 8;
    switch (status)
    {
    case 0:
        sprintf(p, "success");
        break;
    case 1:
        sprintf(p, "stopped");
        break;
    case 2:
        sprintf(p, "infeasible");
        break;
    default:
        sprintf(p, "other (status = %d, status2 = %d)",
            status, status2);
    }
    
    //fprintf(stderr, ">>> sendResult: %lf, %s\n", result, p);
    if (g_bPortMode)
    {
        write_cmd((byte *)buf, p - buf + strlen(p));
    }
}

class MyCbcCompare : public CbcCompareBase
{
public:
    MyCbcCompare(CbcModel *model)
    {
        _model = model;
    }
    bool test(CbcNode *x, CbcNode *y)
    {
        updateIncumbentInCBC(_model);
        return _cmp.test(x, y);
    }
    
    bool newSolution(CbcModel *model, double objectiveAtContinuous,
        int numberInfeasibilitiesAtContinuous)
    {
        _model = model;
        updateIncumbentInCBC(model);
        return _cmp.newSolution(model, objectiveAtContinuous,
            numberInfeasibilitiesAtContinuous);
    }

    bool every1000Nodes(CbcModel *model, int numberNodes)
    {
        _model = model;
        updateIncumbentInCBC(model);
        return _cmp.every1000Nodes(model, numberNodes);
    }

    CbcCompareBase *clone() const
    {
        MyCbcCompare *result = new MyCbcCompare(*this);
        result->_model = _model;
        return result;
    }

private:
    CbcCompareDefault _cmp;
    CbcModel *_model;
};

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

void *readerLoop(void *)
{
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
            fprintf(stderr, ">>> readerLoop(): received best solution: %lf\n", readDouble(buf + 1));
            haveNewIncumbent(readDouble(buf + 1), false);
            break;
        }
    }
    fprintf(stderr, "read_cmd() failed: %d\n", len);
    exit(2);
}

void sendIncumbent(double value)
{
    fprintf(stderr, ">>> sendIncumbent(): %lf\n", value);
    if (g_bPortMode)
    {
        byte buf[1 + 8];
        buf[0] = 3;
        writeDouble(buf + 1, value);
        write_cmd(buf, sizeof(buf));
    }
}

void haveNewIncumbent(double value, bool fromCbc)
{
    pthread_mutex_lock(&g_mutex);
    switch (g_incumbentState)
    {
    case IS_NONE:
        g_incumbentState = fromCbc ? IS_FROM_CBC : IS_FROM_ERL;
        g_incumbentValue = value;
        if (fromCbc)
        {
            sendIncumbent(value);
        }
        break;
    default:
        if (isBetter(g_incumbentValue, value))
        {
            g_incumbentState = fromCbc ? IS_FROM_CBC : IS_FROM_ERL;
            g_incumbentValue = value;
            if (fromCbc)
            {
                sendIncumbent(value);
            }
        }
    }
    pthread_mutex_unlock(&g_mutex);
}

void updateIncumbentInCBC(CbcModel *model)
{
    pthread_mutex_lock(&g_mutex);
    if (g_incumbentState == IS_FROM_ERL)
    {
        fprintf(stderr, ">>> updateIncumbentInCBC(): setting best solution: %lf\n", g_incumbentValue);
        model->setBestSolution(NULL, 0, g_incumbentValue);
        g_incumbentState = IS_FROM_CBC;
    }
    pthread_mutex_unlock(&g_mutex);
}

void processLine(const char *line)
{
    const char *solString = "Integer solution of";
    const char *p = strstr(line, solString);
    if (p != NULL)
    {
        const char *incumbentValue = p + strlen(solString);
        haveNewIncumbent(atof(incumbentValue), true);
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

    char **p = argv;
    for (; *p && strcmp(*p, "--"); ++p)
    {
        if (!strcmp(*p, "-b"))
        {
            g_incumbentState = IS_FROM_CBC;
            g_incumbentValue = atof(*(p + 1));
            model.setBestSolution(NULL, 0, g_incumbentValue);
            ++p;
        }
        if (!strcmp(*p, "-p"))
        {
            g_bPortMode = true;
        }
        if (!strcmp(*p, "-o"))
        {
            logFileName = *(p + 1);
            ++p;
        }
    }

    MyCbcCompare cmp(&model);
    model.setNodeComparison(cmp);

    if (g_bPortMode)
    {
        pthread_t thread;
        if (pthread_create(&thread, NULL, readerLoop, NULL))
        {
            return 1;
        }
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

    fprintf(stderr, ">>> CbcMain: %d %d %d\n", res, model.status(), model.secondaryStatus());

    sendResult(model.status(), model.secondaryStatus(), model.getObjValue());

    return 0;
}

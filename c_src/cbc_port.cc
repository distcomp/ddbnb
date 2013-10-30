#include <pthread.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <fcntl.h>

#include "CbcModel.hpp"
#include "OsiClpSolverInterface.hpp"
#include "CbcCompareDefault.hpp"

typedef unsigned char byte;

static pthread_mutex_t g_mutex = PTHREAD_MUTEX_INITIALIZER;
bool g_bNewBestSolution = false;
double g_dBestSolution;
double g_dOurBestSolution = 1e50;

int read_exact(byte *buf, int len);
int write_exact(byte *buf, int len);
int read_cmd(byte *buf);
int write_cmd(byte *buf, int len);

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

void updateBestSolution(CbcModel *model)
{
    pthread_mutex_lock(&g_mutex);
    if (g_bNewBestSolution && g_dBestSolution < g_dOurBestSolution)
    {
        printf(">>> updateBestSolution(): setting best solution: %lf\n", g_dBestSolution);
        model->setBestSolution(NULL, 0, g_dBestSolution);
        g_bNewBestSolution = false;
    }
    pthread_mutex_unlock(&g_mutex);
}

// void sendSolution(double value, size_t numVars, const double *vars)
// {
//     if (vars == NULL)
//     {
//         return;
//     }
// #ifndef NO_PORT
//     size_t length = 1 + 8 + 8 * numVars;
//     byte *buf = new byte[length];
//     buf[0] = 1;
//     writeDouble(buf + 1, value);
//     for (int i = 0; i < numVars; ++i)
//     {
//         writeDouble(buf + 1 + 8 + i*8, vars[i]);
//     }
//     write_cmd(buf, length);
//     delete [] buf;
// #else
//     printf(">>> sendSolution(): %lf, %ld vars\n", value, numVars);
// #endif // NO_PORT
// }

void sendBestSolutionValue(double value)
{
#ifndef NO_PORT
    byte buf[1 + 8];
    buf[0] = 3;
    writeDouble(buf + 1, value);
    write_cmd(buf, sizeof(buf));
#endif // NO_PORT
    printf(">>> sendBestSolutionValue(): %lf\n", value);

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
        /*case 2:
        sprintf(p, "stopped_on_gap");
        break;
    case 3:
        sprintf(p, "stopped_on_nodes");
        break;
    case 4:
        sprintf(p, "stopped_on_time");
        break;
    case 5:
        sprintf(p, "stopped_on_user_event");
        break;
    case 6:
        sprintf(p, "stopped_on_solutions");
        break;*/
    default:
        sprintf(p, "other (status = %d, status2 = %d)",
            status, status2);
    }
    
#ifndef NO_PORT
    write_cmd((byte *)buf, p - buf + strlen(p));
#else
    printf(">>> sendResult: %s\n", p);
#endif // NO_PORT
}

class MyCbcCompare : public CbcCompareBase
{
public:
    bool test(CbcNode *x, CbcNode *y)
    {
        return _cmp.test(x, y);
    }
    
    bool newSolution(CbcModel *model, double objectiveAtContinuous,
        int numberInfeasibilitiesAtContinuous)
    {
	pthread_mutex_lock(&g_mutex);
	g_dOurBestSolution = model->getObjValue();
	pthread_mutex_unlock(&g_mutex);

        sendBestSolutionValue(model->getObjValue());
        updateBestSolution(model);
        return _cmp.newSolution(model, objectiveAtContinuous,
            numberInfeasibilitiesAtContinuous);
    }

    bool every1000Nodes(CbcModel *model, int numberNodes)
    {
        updateBestSolution(model);
        return _cmp.every1000Nodes(model, numberNodes);
    }

    CbcCompareBase *clone() const
    {
        return new MyCbcCompare(*this);
    }

private:
    CbcCompareDefault _cmp;
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
                printf("Wrong input message size: %d != 9\n", len);
                exit(1);
            }
            pthread_mutex_lock(&g_mutex);
            g_dBestSolution = readDouble(buf + 1);
            printf(">>> readerLoop(): received best solution: %lf\n", g_dBestSolution);
            g_bNewBestSolution = true;
            pthread_mutex_unlock(&g_mutex);
            break;
        }
    }
    printf("read_cmd() failed: %d\n", len);
    exit(2);
}

int main(int argc, char **argv)
{
    if (argc < 2)
    {
        printf("Usage: %s <AMPL stub path> [-o <log file>] [-b <best solution value>] [-- CBC args]\n", argv[0]);
        return 1;
    }

    OsiClpSolverInterface solver;
    CbcModel model(solver);

    for (char **p = argv; *p && strcmp(*p, "--"); ++p)
    {
        if (!strcmp(*p, "-b"))
        {
            model.setBestSolution(NULL, 0, atof(*(p + 1)));
        }
    }

    MyCbcCompare cmp;
    model.setNodeComparison(cmp);

#ifndef NO_PORT
    pthread_t thread;
    if (pthread_create(&thread, NULL, readerLoop, NULL))
    {
        return 1;
    }
#endif // NO_PORT

    FILE *f = NULL;
    for (char **p = argv; *p && strcmp(*p, "--"); ++p)
    {
        if (!strcmp(*p, "-o"))
        {
            f = fopen(*(p + 1), "w");
            if (f == NULL)
            {
                printf("Failed to open %s for writing\n", *(p + 1));
                return 1;
            }
            dup2(fileno(f), 1);
            dup2(fileno(f), 2);
        }
    }

    std::vector<std::string> rawArgs;
    rawArgs.push_back("cbc");
    rawArgs.push_back(argv[1]);
    rawArgs.push_back("-AMPL");
    rawArgs.push_back("wantsol=1");
    char **p = argv;
    for (; *p && strcmp(*p, "--"); ++p) {}
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

    printf("CbcMain: %d %d %d", res, model.status(), model.secondaryStatus());
    
    // if (model.status() == 0)
    // {
    //     sendSolution(model.getObjValue(), model.getNumCols(),
    //         model.bestSolution());
    // }
    sendResult(model.status(), model.secondaryStatus(), model.getObjValue());

    if (f != NULL)
    {
        fclose(f);
    }

    return 0;
}

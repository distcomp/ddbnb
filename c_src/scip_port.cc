#include "ErlPortInterface.h"
#include "reader_nl.h"
#include "event_all.h"

#include "scip/scip.h"
#include "scip/scipdefplugins.h"

#include <stdio.h>
#include <string.h>
#include <fstream>

ErlPortInterface g_portInterface;

static SCIP_RETCODE run(const char *nlfile, const char *logFileName)
{
    SCIP* scip;
    char buffer[SCIP_MAXSTRLEN];

    assert(nlfile != NULL);

    SCIP_CALL( SCIPcreate(&scip) );

    if (logFileName != NULL)
    {
        SCIPsetMessagehdlrLogfile(scip, logFileName);
        SCIPsetMessagehdlrQuiet(scip, true);
    }

    SCIPprintVersion(scip, NULL);
    SCIPinfoMessage(scip, NULL, "\n");

    SCIP_CALL( SCIPincludeDefaultPlugins(scip) );
    SCIP_CALL( SCIPincludeReaderNl(scip) );
    SCIP_CALL( SCIPincludeEventHdlrAll(scip) );

    SCIPprintExternalCodes(scip, NULL);
    SCIPinfoMessage(scip, NULL, "\n");

    SCIPreadParams(scip, "scip.set");

    SCIP_CALL( SCIPreadProb(scip, nlfile, NULL) );
    SCIP_CALL( SCIPsolve(scip) );
    
    SCIP_CALL( SCIPwriteAmplSolReaderNl(scip, NULL) );

    SCIP_SOL *bestSol = SCIPgetBestSol(scip);
    double bestVal = 1e23;
    if (bestSol)
    {
        bestVal = SCIPgetSolOrigObj(scip, bestSol);
    }

    switch (SCIPgetStatus(scip))
    {
    case SCIP_STATUS_OPTIMAL:
        g_portInterface.writeResult("optimal", bestVal);
        break;
    case SCIP_STATUS_INFEASIBLE:
        g_portInterface.writeResult("infeasible", bestVal);
        break;
    default:
        g_portInterface.writeResult("stopped", bestVal);
    }

    SCIP_CALL( SCIPfree(&scip) );

    return SCIP_OKAY;
}

int main(int argc, char **argv)
{
    if (argc < 2)
    {
        fprintf(stderr, "Usage: %s <AMPL stub path> [-p] [-o <log file>] [-b <best solution value>] [-- CBC args]\n", argv[0]);
        return 1;
    }

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

    if (*p)
    {
        std::ofstream f("scip.set");
        for (++p; *p; ++p)
        {
            f << *p << std::endl;
        }
    }

    
    if (haveInitialBestVal)
    {
        g_portInterface.setBestValue(initialBestVal, false);
    }
    g_portInterface.initialize(usePort);

    SCIP_RETCODE retcode = run(argv[1], logFileName);

    if (retcode != SCIP_OKAY)
    {
        SCIPprintError(retcode);
        return -1;
    }

    return 0;
}

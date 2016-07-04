#include "event_all.h"
#include "ErlPortInterface.h"

#include <string.h>
#include <stdio.h>
#include <stdbool.h>

#define EVENTHDLR_NAME         "all"
#define EVENTHDLR_DESC         "event handler for all events"

extern ErlPortInterface g_portInterface;

static
SCIP_DECL_EVENTCOPY(eventCopyAll)
{
    assert(scip != NULL);
    assert(eventhdlr != NULL);
    assert(strcmp(SCIPeventhdlrGetName(eventhdlr), EVENTHDLR_NAME) == 0);
    //SCIP_CALL( SCIPincludeEventHdlrAll(scip) );
    return SCIP_OKAY;
}

static
SCIP_DECL_EVENTINIT(eventInitAll)
{
    assert(scip != NULL);
    assert(eventhdlr != NULL);
    assert(strcmp(SCIPeventhdlrGetName(eventhdlr), EVENTHDLR_NAME) == 0);
    
    SCIP_CALL( SCIPcatchEvent( scip, 0xFFFFFFFF, eventhdlr, NULL, NULL) );
    
    return SCIP_OKAY;
}

static
SCIP_DECL_EVENTEXIT(eventExitAll)
{
    assert(scip != NULL);
    assert(eventhdlr != NULL);
    assert(strcmp(SCIPeventhdlrGetName(eventhdlr), EVENTHDLR_NAME) == 0);
    
    SCIP_CALL( SCIPdropEvent( scip, 0xFFFFFFFF, eventhdlr, NULL, -1) );
    
    return SCIP_OKAY;
}

class SCIPBestValueAcceptor : public BestValueAcceptor
{
    SCIP *_scip;
public:
    SCIPBestValueAcceptor(SCIP *scip)
        : _scip(scip)
    {
    }
    void acceptNewBestValue(double bestVal)
    {
        SCIPsetObjlimit(_scip, bestVal);
    }
};

static
SCIP_DECL_EVENTEXEC(eventExecAll)
{
    assert(eventhdlr != NULL);
    assert(strcmp(SCIPeventhdlrGetName(eventhdlr), EVENTHDLR_NAME) == 0);
    assert(event != NULL);
    assert(scip != NULL);

    if (SCIPeventGetType(event) ==  SCIP_EVENTTYPE_BESTSOLFOUND) {
        SCIPdebugMessage("exec method of event handler for best solution found\n");
        g_portInterface.setBestValue(SCIPgetSolOrigObj(scip, SCIPeventGetSol(event)), true);
    } else {
        SCIPBestValueAcceptor acceptor(scip);
        g_portInterface.getBestValue(acceptor);
    }
    
    return SCIP_OKAY;
}

SCIP_RETCODE SCIPincludeEventHdlrAll(SCIP *scip)
{
    SCIP_EVENTHDLR* eventhdlr = NULL;

    SCIP_CALL( SCIPincludeEventhdlrBasic(scip, &eventhdlr, EVENTHDLR_NAME, EVENTHDLR_DESC,
            eventExecAll, NULL) );
    assert(eventhdlr != NULL);

    SCIP_CALL( SCIPsetEventhdlrCopy(scip, eventhdlr, eventCopyAll) );
    SCIP_CALL( SCIPsetEventhdlrInit(scip, eventhdlr, eventInitAll) );
    SCIP_CALL( SCIPsetEventhdlrExit(scip, eventhdlr, eventExitAll) );

    return SCIP_OKAY;
}

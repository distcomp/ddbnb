/**
 * @file
 * @author Sergey Smirnov <sasmir@gmail.com>
 */

#ifndef __COMMON_H__
#define __COMMON_H__

#include "asl/aslproblem.h"

typedef std::vector<mp::ProblemChanges> VecChanges;

void writeChanged(const std::string &prefix, mp::ASLProblem &p,
    VecChanges changes);

const char *getVarType(const mp::ASLProblem &p, int variable);

bool isInteger(const mp::ASLProblem &p, int variable);

VecChanges splitVariable(const mp::ASLProblem &p,
    const mp::ProblemChanges &changes, int var);

VecChanges splitVariableHalfs(mp::ASLProblem &p,
    const mp::ProblemChanges &changes, int var);

VecChanges splitVariableArgv(mp::ASLProblem &p,
    const mp::ProblemChanges &changes, int argc, char **argv);

std::string baseNameNL(const char *name);

#endif // __COMMON_H__

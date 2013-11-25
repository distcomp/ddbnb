/**
 * @file
 * @author Sergey Smirnov <sasmir@gmail.com>
 */

#ifndef __COMMON_H__
#define __COMMON_H__

#include "solvers/util/problem.h"

typedef std::vector<ampl::ProblemChanges> VecChanges;

void writeChanged(const std::string &prefix, ampl::Problem &p,
    VecChanges changes);

const char *getVarType(const ampl::Problem &p, int variable);

bool isInteger(const ampl::Problem &p, int variable);

VecChanges splitVariable(const ampl::Problem &p,
    const ampl::ProblemChanges &changes, int var);

VecChanges splitVariableHalfs(ampl::Problem &p,
    const ampl::ProblemChanges &changes, int var);

VecChanges splitVariableArgv(ampl::Problem &p,
    const ampl::ProblemChanges &changes, int argc, char **argv);

std::string baseNameNL(const char *name);

#endif // __COMMON_H__

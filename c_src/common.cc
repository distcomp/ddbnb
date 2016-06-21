/**
 * @file
 * @author Sergey Smirnov <sasmir@gmail.com>
 */

#include "common.h"

#include <iostream>
#include <cmath>

typedef std::vector<mp::ProblemChanges> VecChanges;

void writeChanged(const std::string &prefix, mp::ASLProblem &p,
    VecChanges changes)
{
    for (size_t i = 0; i < changes.size(); ++i)
    {
        char buf[256];
        sprintf(buf, "%s_%03lu.nl", prefix.c_str(), i);
        p.WriteNL(buf, &(changes[i]));
        std::cout << "Written " << buf << std::endl;
    }
}

void printLog(int i, int var, double lower, double upper)
{
    std::cout << "Added " << i << " with " << lower
              << " <= var(" << var << ") <= " << upper << std::endl;
}

const char *getVarType(const mp::ASLProblem &p, int variable)
{
    if (p.var_type(variable) == mp::var::CONTINUOUS)
    {
        return "CONTINUOUS";
    }
    return "INTEGER";
}

bool isInteger(const mp::ASLProblem &p, int variable)
{
    return p.var_type(variable) == mp::var::INTEGER;
}

VecChanges splitVariable(const mp::ASLProblem &p,
    const mp::ProblemChanges &changes, int var)
{
    int numValues = (int)(0.5 + (p.var_ub()[var] - p.var_lb()[var] + 1.));
    VecChanges result(numValues, mp::ProblemChanges(p));

    std::vector<double> coefs(p.num_vars(), 0.);
    coefs[var] = 1.;

    for (int i = 0; i < numValues; ++i)
    {
        result[i].AddCon(&(coefs[0]), p.var_lb()[var] + i, p.var_lb()[var] + i);
        printLog(i, var, p.var_lb()[var] + i, p.var_lb()[var] + i);
    }
    return result;
}

VecChanges splitVariableHalfs(mp::ASLProblem &p,
    const mp::ProblemChanges &changes, int var)
{
    double middle = (p.var_ub()[var] - p.var_lb()[var]) / 2.;
    double left = std::floor(p.var_lb()[var] + middle);
    double right = std::floor(p.var_lb()[var] + middle + 1.);

    VecChanges result(2, mp::ProblemChanges(p));
    std::vector<double> coefs(p.num_vars(), 0.);
    coefs[var] = 1.;

    result[0].AddCon(&(coefs[0]), p.var_lb()[var], left);
    printLog(0, var, p.var_lb()[var], left);
    result[1].AddCon(&(coefs[0]), right, p.var_ub()[var]);
    printLog(0, var, right, p.var_ub()[var]);

    return result;
}

VecChanges splitVariableArgv(mp::ASLProblem &p,
    const mp::ProblemChanges &changes, int argc, char **argv)
{
    if (argc < 2)
    {
        std::cout << "Excessive parameters" << std::endl;
        return VecChanges();
    }
    int var = 0;
    if (!sscanf(argv[1], "%d", &var))
    {
        std::cout << "Argument " << argv[1] << " is not integer" << std::endl;
        return VecChanges();
    }
    if (!isInteger(p, var))
    {
        std::cout << "Variable " << var << " is not integer" << std::endl;
        return VecChanges();
    }

    if (!strcmp(argv[0], "split"))
    {
        return splitVariable(p, mp::ProblemChanges(p), var);
    }
    return splitVariableHalfs(p, mp::ProblemChanges(p), var);
}

std::string baseNameNL(const char *name)
{
    std::string result(name);
    size_t delim = result.find_last_of("/\\");
    if (delim != std::string::npos)
    {
        result = result.substr(delim + 1);
    }
    size_t dot = result.find_last_of('.');
    if (dot != std::string::npos)
    {
        result = result.substr(0, dot);
    }
    return result;
}

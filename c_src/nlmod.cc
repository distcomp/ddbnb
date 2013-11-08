#include "common.h"

#include "solvers/util/problem.h"
#include <iostream>

int main(int argc, char **argv)
{
    if (argc != 2 && argc % 2 != 0)
    {
        std::cout << "Usage: " << argv[0]
                  << " <stub>.nl [<split | halfs> <variable number>]*" << std::endl;
        return 1;
    }

    ampl::Problem p;
    p.Read(argv[1]);
    
    if (argc == 2)
    {
        std::cout << "VarN\tLB\tUB\tType" << std::endl;
        for (int i = 0; i < p.num_vars(); ++i)
        {
            std::cout << i << "\t" << var_name_ASL((ASL*)p.asl_, i) << "\t" << p.var_lb(i) << "\t" << p.var_ub(i) << "\t"
                      << getVarType(p, i) << std::endl;
        }
        return 0;
    }


    std::string baseName(baseNameNL(argv[1]));
    argc -= 2;
    argv += 2;

    VecChanges previous(1, ampl::ProblemChanges(p));
    while (argc >= 2)
    {
        VecChanges updated;
        for (int i = 0; i < previous.size(); ++i)
        {
            VecChanges tmp(splitVariableArgv(p, previous[i], argc, argv));
            updated.insert(updated.end(), tmp.begin(), tmp.end());
        }
        previous = updated;
        argc -= 2;
        argv += 2;
    }
    writeChanged(baseName, p, previous);

    return 0;
}

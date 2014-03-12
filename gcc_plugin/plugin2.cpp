
// GCC header includes to get the parse tree
// declarations. The order is important and
// doesn't follow any kind of logic.
//

#include <stdlib.h>
#include <gmp.h>

#include <cstdlib> // Include before GCC poisons
                   // some declarations.

#include "gcc-plugin.h"

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "intl.h"

#include "tm.h"

#include "cp/cp-tree.h"
#include "c-family/c-common.h"
#include "c-family/c-pragma.h"
#include "diagnostic.h"
#include "tree-iterator.h"

#include <set>
#include <map>
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <functional>
#include <assert.h>

std::ofstream ofstr;



int plugin_is_GPL_compatible;

struct decl_comparator
{
    bool
    operator() (tree x, tree y) const
    {
        location_t xl (DECL_SOURCE_LOCATION (x));
        location_t yl (DECL_SOURCE_LOCATION (y));

        return xl < yl;
    }
};

typedef std::multiset<tree, decl_comparator> decl_set;

struct Namespace;
using NamespaceChildren = std::map<std::string, Namespace>;
struct Namespace
{
    NamespaceChildren m_children;
};


Namespace g_namespace;


// struct ParseContext;
// using SubParseContexts = std::multiset<int, ParseContext>

// struct ParseContext
// {
//     bool Descend (int tree_code, const tree* tree, int level);
//     SubParseContexts m_SubParseContexts;

//     static ParseContext g_context;
// };


const char* indent_spaces =
    "                                                                       "
    "                                                                       "
    "                                                                       "
    "                                                                       "
    "                                                                       "
    "                                                                       "
    "                                                                       "
    "                                                                       "
    "                                                                       "
    "                                                                       "
    "                                                                       "
    ;


std::string ExprLoc (tree decl)
{
    if (EXPR_HAS_LOCATION(decl))
    {
        std::ostringstream ostr;
        ostr << EXPR_FILENAME(decl) << ':' << EXPR_LINENO(decl);
        return ostr.str();
    }

    return "NO LOC";
}


tree g_body;

int indent_cnt = 3;

enum TreePrintOptions
{
    CALL_REFS = 1,
    TRACE     = 2,
    ALL       = 3
};

struct PrintTreeContext
{
    PrintTreeContext (
        TreePrintOptions printOpts,
        std::ostream& ostr) :
     m_printOpts (printOpts),
     m_ostr (ostr)
    { }

    ~PrintTreeContext()
    { }

    bool trace() const
    {
        return m_printOpts & TreePrintOptions::TRACE;
    }

    bool call_refs() const
    {
        return m_printOpts & TreePrintOptions::CALL_REFS;
    }


    TreePrintOptions m_printOpts;
    std::ostream& m_ostr;
    bool m_insideFunction = false;
    int m_delegateSearch = 0;

    std::string m_delegateTargetFunction;
    std::string m_delegateVariable;

    std::vector<std::string> m_expressionLocs;
};

PrintTreeContext *g_printTreeContext;

void print_tree (tree decl, int level, PrintTreeContext &ptc);

void
collect (tree ns, decl_set& set)
{
    tree decl;
    cp_binding_level* level (NAMESPACE_LEVEL (ns));

    // Collect declarations.
    //
    for (decl = level->names; decl != 0; decl = TREE_CHAIN (decl))
    {
        // int tc (TREE_CODE (decl));
        // std::cout << __FILE__ << ':' << __LINE__ << ' ' << tree_code_name[tc]
        //           << ' ' << DECL_SOURCE_LOCATION(decl)
        //           << ' ' << DECL_SOURCE_FILE (decl) << ":"
        //           << DECL_SOURCE_LINE (decl) << std::endl;
        // int tc (TREE_CODE (decl));

        // tree id (DECL_NAME (decl));

        // const char* id_ptr = (id != 0 ? IDENTIFIER_POINTER (id) : "<unnamed>");
        // id_ptr = id_ptr ? id_ptr : "<nullptr>";

        // std::cout << __FILE__ << ':' << __LINE__ << ' ' << tree_code_name[tc];
        // if (id_ptr)
        //     std::cout << " id:" << id_ptr;

        // if (DECL_SOURCE_LOCATION(decl) != 0)
        // {
        //     std::cout << ' ' << DECL_SOURCE_LOCATION(decl)
        //               << ' ' << DECL_SOURCE_FILE (decl) << ":"
        //               << DECL_SOURCE_LINE (decl);
        // }

        // std::cout << std::endl;

        if (DECL_IS_BUILTIN (decl))
            continue;

        set.insert (decl);

        print_tree (decl, 0, *g_printTreeContext);
    }

    // Traverse namespaces.
    //
    for (decl = level->namespaces; decl != 0; decl = TREE_CHAIN (decl))
    {
        // int tc (TREE_CODE (decl));
        // std::cout << __FILE__ << ':' << __LINE__ << ' ' << tree_code_name[tc]
        //           << ' ' << DECL_SOURCE_LOCATION(decl)
        //           << ' ' << DECL_SOURCE_FILE (decl) << ":"
        //           << DECL_SOURCE_LINE (decl);

        // tree id (DECL_NAME (decl));

        // const char* id_ptr = (id != 0 ? IDENTIFIER_POINTER (id) : "<unnamed>");
        // id_ptr = id_ptr ? id_ptr : "<nullptr>";

        // if (id_ptr)
        //     std::cout << " id:" << id_ptr;

        // std::cout << std::endl;

        if (DECL_IS_BUILTIN (decl))
            continue;

        collect (decl, set);
    }

}

// std::string
// decl_namespace (tree decl)
// {
//     std::string s, tmp;

//     for (tree scope (CP_DECL_CONTEXT (decl));
//          scope != global_namespace;
//          scope = CP_DECL_CONTEXT (scope))
//     {
//         tree id (DECL_NAME (scope));
//         std::cout << scope << std::endl;
//         std::cout << id << std::endl;

//         if (id != 0)
//             std::cout << (void *)IDENTIFIER_POINTER (id) << std::endl;

//         const char* id_ptr = (id != 0 ? IDENTIFIER_POINTER (id) : "<unnamed>");
//         id_ptr = id_ptr ? id_ptr : "<nullptr>";

//         tmp = "::";
//         tmp += id_ptr;
//         s = tmp;
//     }

//     return s;
// }


std::string
decl_scope (tree decl, Namespace* ns)
{
    std::string s, tmp;
    std::vector<std::string> namespaces;

    for (tree scope (CP_DECL_CONTEXT (decl));
         scope != global_namespace && scope;
         scope = CP_DECL_CONTEXT (scope))
    {
        g_body = scope;
        if (TREE_CODE(scope) == RECORD_TYPE ||
            TREE_CODE(scope) == ENUMERAL_TYPE)
        {
            if (TYPE_NAME (scope))
                scope = TYPE_NAME (scope);
            //tmp_scope = TYPE_CANONICAL(scope);
            else
                scope = TYPE_NAME (TYPE_CANONICAL(scope));
            // if (tmp_scope)
            //     scope
        }
        tree id = 0;
        if (scope)
            id = DECL_NAME (scope);

        const char* id_ptr = (id != 0 ? IDENTIFIER_POINTER (id) : "<unnamed>");
        id_ptr = id_ptr ? id_ptr : "<nullptr>";

        namespaces.push_back (id_ptr);

        tmp = "::";
        tmp += id_ptr;
        tmp += s;
        s.swap (tmp);
        if (!scope)
            break;
    }

    // for (auto ritr = namespaces.rbegin(); ritr != namespaces.rend(); ++ritr)
    // {
    //     auto ns_itr = ns->m_children.find (*ritr);
    //     if (ns_itr == ns->m_children.end())
    //         ns_itr = ns->m_children.insert (NamespaceChildren::value_type(*ritr, Namespace())).first;

    //     ns = &ns_itr->second;
    // }

    return s;
}


void
print_decl (tree decl, Namespace* ns, std::ostream& ostr)
{
    int tc (TREE_CODE (decl));
    tree id (DECL_NAME (decl));
    const char* name (id ? IDENTIFIER_POINTER (id) : "<unnamed>");

    int name_len = strlen (name);
    while (name[name_len-1] == ' ' && name_len > 0) --name_len;

    std::string scope_name = decl_scope (decl, ns);
    ostr << tree_code_name[tc] << " "
         << scope_name << "::";

    ostr.write (name, name_len);
}

template <class CallbackT>
void
print_decl (tree decl, Namespace* ns, std::ostream& ostr, CallbackT cb)
{
    int tc (TREE_CODE (decl));
    tree id (DECL_NAME (decl));
    const char* name (id ? IDENTIFIER_POINTER (id) : "<unnamed>");

    int name_len = strlen (name);
    while (name[name_len-1] == ' ' && name_len > 0) --name_len;

    std::string scope_name = decl_scope (decl, ns);
    ostr << tree_code_name[tc] << " "
         << scope_name << "::";
    ostr.write (name, name_len);

    cb (scope_name, name);
    // if (ptc.call_refs() &&
    //     strncmp (scope_name.c_str(), "fastdelegate", 12) == 0 &&
    //     strncmp (name, "operator", 8) == 0)
    // {
    //     ptc.m_ostr << 
    // }
}


int check_if_delegate (
    const std::string& scope_name,
    const std::string& name,
    PrintTreeContext& ptc,
    int &mode)
{
    if (strncmp (scope_name.c_str(), "::fastdelegate", 14) == 0 &&
        strncmp (name.c_str(), "operator", 8) == 0)
    {
        if (name[8] == '=' && name[9] == '\0')
            mode = 1;
        else if (name[8] == '(' && name[9] == ')' && name[10] == '\0')
            mode = 2;
    }

    // ptc.m_ostr << " scope_name=\"" << scope_name
    //            << "\" name=\"" << name
    //            << "\" " << mode << ' ';

    return mode;
}


void print_tree (tree decl, int level, PrintTreeContext &ptc)
{
    if (!decl)
        return;

    if (level > 1000)
    {
        g_body = decl;
        asm volatile ("int3");
        return;
    }

    // g_body = decl;
    if (TREE_CODE(decl) == FUNCTION_DECL)
    {
        if (ptc.trace())
        {
            ptc.m_ostr.write (indent_spaces, indent_cnt * level);
            print_decl (decl, &g_namespace, ptc.m_ostr);
            ptc.m_ostr << " " << DECL_SOURCE_FILE (decl) << ":"
                       << DECL_SOURCE_LINE (decl) << std::endl;
            for (tree parm = DECL_ARGUMENTS(decl);
                 parm != 0; parm = TREE_CHAIN (parm))
            {
                print_tree (parm, level+1, ptc);
            }
        }

        if (DECL_SAVED_TREE(decl) && level == 0)
        {
            if (ptc.call_refs())
            {
                ptc.m_ostr << (ptc.m_insideFunction ? "REF ": "DEF ");
                print_decl (decl, &g_namespace, ptc.m_ostr);

                tree parm_list = DECL_ARGUMENTS(decl);

                if (parm_list)
                    ptc.m_ostr << ',' << list_length(parm_list);

                ptc.m_ostr << ' ' << DECL_SOURCE_FILE (decl) << ':'
                           << DECL_SOURCE_LINE (decl);

                if (!ptc.m_expressionLocs.empty())
                    ptc.m_ostr << ' ' << ptc.m_expressionLocs.back();
                ptc.m_ostr << std::endl;
            }
            tree init_stmt = DECL_SAVED_TREE(decl);
            // ptc.m_insideFunction = true;
            print_tree (init_stmt, level+1, ptc);
            // ptc.m_insideFunction = false;
        }
        else if (/*ptc.m_insideFunction &&*/ ptc.call_refs())
        {
            ptc.m_ostr << "REF ";
            int mode = 0;
            print_decl (decl, &g_namespace, ptc.m_ostr,
                        std::bind (&check_if_delegate,
                                   std::placeholders::_1,
                                   std::placeholders::_2,
                                   ptc, std::ref (mode)));

            tree parm_list = DECL_ARGUMENTS(decl);
            if (parm_list)
                ptc.m_ostr << ',' << list_length(parm_list);

            ptc.m_ostr << ' ' << DECL_SOURCE_FILE (decl) << ':'
                       << DECL_SOURCE_LINE (decl);

            if (!ptc.m_expressionLocs.empty())
                ptc.m_ostr << ' ' << ptc.m_expressionLocs.back();
            ptc.m_ostr << std::endl;
            if (mode)
            {
                assert (ptc.m_delegateSearch == 0);
                ptc.m_delegateSearch = mode;
            }
        }

        // std::cout.write (indent_spaces, indent_cnt * level);
        // print_decl (decl, &g_namespace);
        // std::cout << std::endl;
        return;
    }

    if (TREE_CODE(decl) == VAR_DECL ||
        TREE_CODE(decl) == FIELD_DECL)
    {
        if (ptc.trace())
        {
            ptc.m_ostr.write (indent_spaces, indent_cnt * level);
            print_decl (decl, &g_namespace, ptc.m_ostr);
            ptc.m_ostr << " " << DECL_SOURCE_FILE (decl) << ":"
                       << DECL_SOURCE_LINE (decl) << std::endl;
        }

        // if (DECL_SAVED_TREE(decl) && level == 0)
        // {
        //     if (ptc.call_refs())
        //     {
        //         ptc.m_ostr << (ptc.m_insideFunction ? "REF ": "DEF ");
        //         print_decl (decl, &g_namespace, ptc.m_ostr);
        //         // ptc.m_ostr << ' ' << ExprLoc(decl);
        //         if (!ptc.m_expressionLocs.empty())
        //             ptc.m_ostr << ' ' << ptc.m_expressionLocs.back();
        //         ptc.m_ostr << std::endl;
        //     }
        //     tree init_stmt = DECL_SAVED_TREE(decl);
        //     // ptc.m_insideFunction = true;
        //     print_tree (init_stmt, level+1, ptc);
        //     // ptc.m_insideFunction = false;
        // }
        // else if (/*ptc.m_insideFunction &&*/ ptc.call_refs())
        // {
        //     ptc.m_ostr << "REF ";
        //     print_decl (decl, &g_namespace, ptc.m_ostr);
        //     // ptc.m_ostr << ' ' << ExprLoc(decl);
        //     if (!ptc.m_expressionLocs.empty())
        //         ptc.m_ostr << ' ' << ptc.m_expressionLocs.back();
        //     ptc.m_ostr << std::endl;
        // }

        // std::cout.write (indent_spaces, indent_cnt * level);
        // print_decl (decl, &g_namespace);
        // std::cout << std::endl;
        return;
    }

    if (TREE_CODE(decl) == STATEMENT_LIST)
    {
        int cnt = 0;
        for (auto tsi = tsi_start(decl); !tsi_end_p(tsi);
             ++cnt, tsi_next(&tsi))
        {
            auto ptree = tsi_stmt_ptr(tsi);
            if (ptc.trace())
            {
                ptc.m_ostr.write (indent_spaces, indent_cnt * level);
                ptc.m_ostr << cnt << ": " << tree_code_name[TREE_CODE(*ptree)]
                           << " op_cnt=" << TREE_OPERAND_LENGTH(*ptree) << ' '
                           << ExprLoc(*ptree) << std::endl;
            }
            for (int i = 0; i < TREE_OPERAND_LENGTH(*ptree); ++i)
            {
                tree top = TREE_OPERAND (*ptree, i);
                print_tree (top, level+1, ptc);
            }
        }
        return;
    }

    // if (TREE_CODE(decl) == PARM_DECL)
    // {
    //     if (ptc.trace())
    //     {
    //         function_args_iterator fai;
    //         tree parm;
    //         ptc.m_ostr.write (indent_spaces, indent_cnt * level);
    //         ptc.m_ostr << "params" << std::endl;
    //         for (tree parm = decl; parm != 0; parm = TREE_CHAIN (parm))
    //         {
    //             print_tree (parm, level+1, ptc);
    //         }
    //     }
    //     // for (auto tsi = tsi_start(decl); !tsi_end_p(tsi);
    //     //      ++cnt, tsi_next(&tsi))
    //     // {
    //     //     auto ptree = tsi_stmt_ptr(tsi);
    //     //     if (ptc.trace())
    //     //     {
    //     //         ptc.m_ostr.write (indent_spaces, indent_cnt * level);
    //     //         ptc.m_ostr << cnt << ": " << tree_code_name[TREE_CODE(*ptree)]
    //     //                    << " op_cnt=" << TREE_OPERAND_LENGTH(*ptree) << ' '
    //     //                    << ExprLoc(*ptree) << std::endl;
    //     //     }
    //     //     for (int i = 0; i < TREE_OPERAND_LENGTH(*ptree); ++i)
    //     //     {
    //     //         tree top = TREE_OPERAND (*ptree, i);
    //     //         print_tree (top, level+1, ptc);
    //     //     }
    //     // }
    //     return;
    // }

    if (TREE_CODE(decl) == EXPR_STMT)
    {
        if (ptc.trace())
        {
            ptc.m_ostr.write (indent_spaces, indent_cnt * level);
            ptc.m_ostr << tree_code_name[TREE_CODE(decl)] << ' '
                       << ExprLoc(decl) << std::endl;
        }
        tree expr_tree = EXPR_STMT_EXPR(decl);
        print_tree (expr_tree, level+1, ptc);
        return;
    }

    if (TREE_CODE(decl) == CALL_EXPR)
    {
        if (ptc.trace())
        {
            ptc.m_ostr.write (indent_spaces, indent_cnt * level);
            ptc.m_ostr << tree_code_name[TREE_CODE(decl)] << ' '
                       << ExprLoc(decl) << std::endl;
        }

        bool isFastDelegate = false;
        int priorDelegateSearch = 0;
        for (int i = 0; i < TREE_OPERAND_LENGTH(decl); ++i)
        {
            tree operand = TREE_OPERAND (decl, i);

            // g_body = operand;
            // asm volatile ("int3");

            if (operand)
            {
                if (priorDelegateSearch == 0 && ptc.m_delegateSearch != 0 &&
                    ptc.call_refs())
                {
                    switch (ptc.m_delegateSearch)
                    {
                        case 0: break;
                        case 1:
                        case 2:
                        {
                            if (TREE_OPERAND_LENGTH(operand) > 0)
                            {
                                bool found = false;
                                g_body = operand;
                                tree comp_ref_v = TREE_OPERAND (operand, 0);
                                if (comp_ref_v && TREE_CODE(comp_ref_v) == COMPONENT_REF)
                                {
                                    tree delegate_decl_v = TREE_OPERAND (comp_ref_v, 1);
                                    if (delegate_decl_v && TREE_OPERAND (delegate_decl_v, 1))
                                    {
                                        std::ostringstream ostr;
                                        print_decl (delegate_decl_v, &g_namespace, ostr);
                                        ostr << " " << DECL_SOURCE_FILE (delegate_decl_v) << ":"
                                             << DECL_SOURCE_LINE (delegate_decl_v) << std::endl;

                                        if (!ptc.m_delegateVariable.empty())
                                            ptc.m_delegateVariable += " append ";
                                        ptc.m_delegateVariable = ostr.str();
                                        priorDelegateSearch = ptc.m_delegateSearch;
                                    }
                                    else
                                    {
                                        ptc.m_ostr << "WARN " << __FILE__ << ':' << __LINE__
                                                   << " unexpected tree code type." << std::endl;
                                    }
                                }
                                else
                                {
                                    ptc.m_ostr << "WARN " << __FILE__ << ':' << __LINE__
                                               << " unexpected tree code type." << std::endl;
                                }
                            }
                            else
                            {
                                ptc.m_ostr << "WARN " << __FILE__ << ':' << __LINE__
                                           << " expected at least one operand." << std::endl;
                            }

                            break;
                        }
                        default:
                            assert (false);
                            break;
                    }
                }

                bool pushed = false;
                if (i == 1 && EXPR_HAS_LOCATION(decl))
                {
                    std::ostringstream ostr;
                    ostr << EXPR_FILENAME(decl) << ':' << EXPR_LINENO(decl);
                    ptc.m_expressionLocs.push_back (ostr.str());
                    pushed = true;
                }
                print_tree (operand, level+1, ptc);
                if (pushed)
                {
                    ptc.m_expressionLocs.pop_back();
                }
            }
        }
        if (ptc.call_refs())
        {
            switch (ptc.m_delegateSearch)
            {
                case 1:
                    if (ptc.m_delegateVariable.empty() ||
                        ptc.m_delegateTargetFunction.empty())
                    {
                        ptc.m_ostr << "WARN in delegate assign context, but "
                                   << "missing variable and/or target function"
                                   << std::endl;
                    }
                    else
                    {
                        ptc.m_ostr << "DLGT_ASSIGN " << ptc.m_delegateVariable << ' '
                                   << ptc.m_delegateTargetFunction << std::endl;
                    }
                    break;
                case 2:
                    if (ptc.m_delegateVariable.empty())
                    {
                        ptc.m_ostr << "WARN in delegate invocation context, but "
                                   << "missing variable function" << std::endl;
                    }
                    else
                    {
                        ptc.m_ostr << "DLGT_INVOKE " << ptc.m_delegateVariable
                                   << ' ' << std::endl;
                    }
                    break;
            }
        }

        ptc.m_delegateTargetFunction.clear();
        ptc.m_delegateVariable.clear();
        ptc.m_delegateSearch = 0;
        return;
    }

    if (TREE_CODE(decl) == PTRMEM_CST)
    {
        if (ptc.trace())
        {
            ptc.m_ostr.write (indent_spaces, indent_cnt * level);
            ptc.m_ostr << tree_code_name[TREE_CODE(decl)] << ' ';
            print_decl (PTRMEM_CST_MEMBER(decl), &g_namespace, ptc.m_ostr);
            ptc.m_ostr << " " << DECL_SOURCE_FILE (PTRMEM_CST_MEMBER(decl))
                       << ":" << DECL_SOURCE_LINE (PTRMEM_CST_MEMBER(decl));
            ptc.m_ostr << ' ';
            ptc.m_ostr << ExprLoc(decl) << std::endl;
            // PTRMEM_CST_CLASS(decl)
        }
        if (ptc.m_delegateSearch == 1)
        {
            std::ostringstream ostr;
            print_decl (PTRMEM_CST_MEMBER(decl), &g_namespace, ostr);
            ostr << " " << DECL_SOURCE_FILE (PTRMEM_CST_MEMBER(decl))
                 << ":" << DECL_SOURCE_LINE (PTRMEM_CST_MEMBER(decl));
            assert (ptc.m_delegateTargetFunction.empty());
            ptc.m_delegateTargetFunction = ostr.str();
        }
        return;
    }

    if (TREE_CODE(decl) == BIND_EXPR)
    {
        if (ptc.trace())
        {
            ptc.m_ostr.write (indent_spaces, indent_cnt * level);
            ptc.m_ostr << tree_code_name[TREE_CODE(decl)] << ' '
                       << ExprLoc(decl) << std::endl;
        }

        tree bev = BIND_EXPR_VARS(decl);
        print_tree (bev, level+1, ptc);
        // tree bebb = BIND_EXPR_BODY_BLOCK(decl);
        // print_tree (bebb, level+1, ptc);
        tree beb = BIND_EXPR_BODY(decl);
        print_tree (beb, level+1, ptc);

        return;
    }

    // using EXPR_P() causes infinite recursion
    if (EXPRESSION_CLASS_P(decl) ||
        UNARY_CLASS_P(decl) ||
        VL_EXP_CLASS_P(decl) ||
        EXPRESSION_CLASS_P(decl) ||
        REFERENCE_CLASS_P(decl))
    {
        if (ptc.trace())
        {
            ptc.m_ostr.write (indent_spaces, indent_cnt * level);
            ptc.m_ostr << tree_code_name[TREE_CODE(decl)] << ' '
                       << ExprLoc(decl) << std::endl;
        }
        for (int i = 0; i < TREE_OPERAND_LENGTH(decl); ++i)
        {
            tree operand = TREE_OPERAND (decl, i);
            print_tree (operand, level+1, ptc);
        }
        return;
    }

    if (ptc.trace())
    {
        ptc.m_ostr.write (indent_spaces, indent_cnt * level);
        ptc.m_ostr << tree_code_name[TREE_CODE(decl)] << " is unhandled "
                   << ExprLoc(decl) << std::endl;
    }

    return;
}


const char* dump_spaces= "                                               ";

void dump_namespace (Namespace& ns, int depth)
{
    for (auto &ns_child: ns.m_children)
    {
        std::cout.write (dump_spaces, depth*3);
        std::cout << ns_child.first << std::endl;
        dump_namespace (ns_child.second, depth+1);
    }
}


void
traverse (tree ns)
{
    decl_set set;
    collect (ns, set);
    // std::cout << "done with collect" << std::endl;

    // for (decl_set::iterator i (set.begin ()), e (set.end ());
    //      i != e; ++i)
    // {
    //     print_decl (*i, &g_namespace, std::cout);
    //     std::cout << std::endl;
    // }

    // dump_namespace (g_namespace, 0);
}

extern "C" void
gate_callback (void* arg1, void* arg2)
{
    // If there were errors during compilation,
    // let GCC handle the exit.
    //
    if (errorcount || sorrycount)
        return;

    // g_printTreeContext->m_ostr
    //     << "gate_callback " << arg1 << ' ' << arg2 << std::endl;

    tree tree_arg = (tree) arg1;
    // g_body = tree_arg;
    // asm volatile ("int3");

    // TreePrintOptions tpo = TreePrintOptions::CALL_REFS;
    // // if (TREE_CODE(tree_arg) == FUNCTION_DECL && DECL_NAME(tree_arg) &&
    // //     strcmp (IDENTIFIER_POINTER(DECL_NAME(tree_arg)), "main") == 0)
    // // {
    // //     tpo = TreePrintOptions::ALL;
    // // }
    // g_printTreeContext->m_printOpts = tpo;
    // g_printTreeContext->m_ostr << "BEG" << std::endl;
    print_tree (tree_arg, 0, *g_printTreeContext);
    // g_printTreeContext->m_ostr << "END" << std::endl;

    int r (0);

    //
    // Process AST. Issue diagnostics and set r
    // to 1 in case of an error.
    //
    // std::cout << "processing " << main_input_filename << std::endl;
    // traverse (global_namespace);
    // exit (r);
}

extern "C" void
gcc_finished (void*, void*)
{
    delete g_printTreeContext;
}

extern "C" int
plugin_init (plugin_name_args* info,
             plugin_gcc_version* ver)
{
    int r (0);

    // std::cout << "starting " << info->base_name << std::endl;

    std::string dst_filename;
    for (int i = 0; i < info->argc; ++i)
    {
        if (strlen (info->argv[i].key) == 3 &&
            strncmp (info->argv[i].key, "dst", 3) == 0)
        {
            dst_filename = info->argv[i].value;
        }
    }
    //
    // Parse options if any.
    //
    ofstr.open(dst_filename.c_str(), std::ofstream::out | std::ofstream::trunc);
    if (!ofstr)
    {
        std::cout << "unable to open output file" << std::endl;
        r = -1;
    }

    if (r == 0)
    {
        g_printTreeContext =
            new PrintTreeContext {TreePrintOptions::ALL, ofstr};
            // new PrintTreeContext {TreePrintOptions::ALL, std::cout};

        // Disable assembly output.
        //
        asm_file_name = HOST_BIT_BUCKET;

        // Register callbacks.
        //
        register_callback (info->base_name,
                           //PLUGIN_OVERRIDE_GATE,
                           PLUGIN_PRE_GENERICIZE,
                           &gate_callback,
                           0);

        register_callback (info->base_name,
                           PLUGIN_FINISH,
                           &gcc_finished,
                           0);
    }
    return r;
}

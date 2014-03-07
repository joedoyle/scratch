
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
     m_ostr (ostr),
     m_insideFunction (false)
    { }

    ~PrintTreeContext()
    {
    }

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
    bool m_insideFunction;

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
         scope != global_namespace;
         scope = CP_DECL_CONTEXT (scope))
    {
        if (TREE_CODE(scope) == RECORD_TYPE ||
            TREE_CODE(scope) == ENUMERAL_TYPE)
            scope = TYPE_NAME (scope);

        tree id (DECL_NAME (scope));

        const char* id_ptr = (id != 0 ? IDENTIFIER_POINTER (id) : "<unnamed>");
        id_ptr = id_ptr ? id_ptr : "<nullptr>";

        namespaces.push_back (id_ptr);

        tmp = "::";
        tmp += id_ptr;
        tmp += s;
        s.swap (tmp);
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

    ostr << tree_code_name[tc] << " "
         << decl_scope (decl, ns) << "::"
         << name
         << " " << DECL_SOURCE_FILE (decl) << ":"
         << DECL_SOURCE_LINE (decl);// << std::endl;
}


void print_tree (tree decl, int level, PrintTreeContext &ptc)
{
    if (!decl)
        return;

    if (level > 50)
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
            ptc.m_ostr << std::endl;
        }

        if (DECL_SAVED_TREE(decl) && level == 0)
        {
            if (ptc.call_refs())
            {
                ptc.m_ostr << (ptc.m_insideFunction ? "REF ": "DEF ");
                print_decl (decl, &g_namespace, ptc.m_ostr);
                // ptc.m_ostr << ' ' << ExprLoc(decl);
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
            print_decl (decl, &g_namespace, ptc.m_ostr);
            // ptc.m_ostr << ' ' << ExprLoc(decl);
            if (!ptc.m_expressionLocs.empty())
                ptc.m_ostr << ' ' << ptc.m_expressionLocs.back();
            ptc.m_ostr << std::endl;
        }

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
        for (int i = 0; i < TREE_OPERAND_LENGTH(decl); ++i)
        {
            tree operand = TREE_OPERAND (decl, i);

            // g_body = operand;
            // asm volatile ("int3");

            if (operand)
            {
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

    // rather than or'ing these all together
    if (EXPRESSION_CLASS_P(decl) ||
        UNARY_CLASS_P(decl))
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

    TreePrintOptions tpo = TreePrintOptions::CALL_REFS;
    // if (TREE_CODE(tree_arg) == FUNCTION_DECL && DECL_NAME(tree_arg) &&
    //     strcmp (IDENTIFIER_POINTER(DECL_NAME(tree_arg)), "main") == 0)
    // {
    //     tpo = TreePrintOptions::ALL;
    // }
    g_printTreeContext->m_printOpts = tpo;
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

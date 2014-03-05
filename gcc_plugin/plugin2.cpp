
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

tree g_body;

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
        // std::cerr << __FILE__ << ':' << __LINE__ << ' ' << tree_code_name[tc]
        //           << ' ' << DECL_SOURCE_LOCATION(decl)
        //           << ' ' << DECL_SOURCE_FILE (decl) << ":"
        //           << DECL_SOURCE_LINE (decl) << std::endl;
        int tc (TREE_CODE (decl));

        tree id (DECL_NAME (decl));

        const char* id_ptr = (id != 0 ? IDENTIFIER_POINTER (id) : "<unnamed>");
        id_ptr = id_ptr ? id_ptr : "<nullptr>";

        std::cerr << __FILE__ << ':' << __LINE__ << ' ' << tree_code_name[tc];
        if (id_ptr)
            std::cerr << " id:" << id_ptr;

        if (DECL_SOURCE_LOCATION(decl) != 0)
        {
            std::cerr << ' ' << DECL_SOURCE_LOCATION(decl)
                      << ' ' << DECL_SOURCE_FILE (decl) << ":"
                      << DECL_SOURCE_LINE (decl);
        }

        std::cerr << std::endl;

        if (DECL_IS_BUILTIN (decl))
            continue;

        std::cerr << __FILE__ << ':' << __LINE__ << std::endl;
        set.insert (decl);
        std::cerr << __FILE__ << ':' << __LINE__ << std::endl;

        if (FUNCTION_TYPE_CHECK(decl))
        {
            std::cerr << __FILE__ << ':' << __LINE__ << std::endl;


            {
                tree scratch = DECL_SAVED_TREE(decl);
                g_body = decl;
                asm volatile ("int3");

                tree id (DECL_NAME (scratch));

                const char* id_ptr = (id != 0 ? IDENTIFIER_POINTER (id) : "<unnamed>");
                id_ptr = id_ptr ? id_ptr : "<nullptr>";

                std::cerr << __FILE__ << ':' << __LINE__ << ' '
                          << tree_code_name[TREE_CODE(scratch)];
                if (id_ptr)
                    std::cerr << " id:" << id_ptr;

                if (DECL_SOURCE_LOCATION(scratch) != 0)
                {
                    std::cerr << ' ' << DECL_SOURCE_LOCATION(scratch)
                              << ' ' << DECL_SOURCE_FILE (scratch) << ":"
                              << DECL_SOURCE_LINE (scratch);
                }

                std::cerr << std::endl;
            }
        }
    }

    // Traverse namespaces.
    //
    for(decl = level->namespaces; decl != 0; decl = TREE_CHAIN (decl))
    {
        // int tc (TREE_CODE (decl));
        // std::cerr << __FILE__ << ':' << __LINE__ << ' ' << tree_code_name[tc]
        //           << ' ' << DECL_SOURCE_LOCATION(decl)
        //           << ' ' << DECL_SOURCE_FILE (decl) << ":"
        //           << DECL_SOURCE_LINE (decl) << std::endl;

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
//         std::cerr << scope << std::endl;
//         std::cerr << id << std::endl;

//         if (id != 0)
//             std::cerr << (void *)IDENTIFIER_POINTER (id) << std::endl;

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
        if (TREE_CODE (scope) == RECORD_TYPE || TREE_CODE(scope) == ENUMERAL_TYPE)
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

    for (auto ritr = namespaces.rbegin(); ritr != namespaces.rend(); ++ritr)
    {
        auto ns_itr = ns->m_children.find (*ritr);
        if (ns_itr == ns->m_children.end())
            ns_itr = ns->m_children.insert (NamespaceChildren::value_type(*ritr, Namespace())).first;

        ns = &ns_itr->second;
    }

    return s;
}

void
print_decl (tree decl, Namespace* ns)
{
    int tc (TREE_CODE (decl));
    tree id (DECL_NAME (decl));
    const char* name (id ? IDENTIFIER_POINTER (id) : "<unnamed>");

    std::cerr << tree_code_name[tc] << " "
              << decl_scope (decl, ns) << "::"
              << name
              << " at " << DECL_SOURCE_FILE (decl) << ":"
              << DECL_SOURCE_LINE (decl) << std::endl;
}

const char* dump_spaces= "                                               ";

void dump_namespace (Namespace& ns, int depth)
{
    for (auto &ns_child: ns.m_children)
    {
        std::cerr.write (dump_spaces, depth*3);
        std::cerr << ns_child.first << std::endl;
        dump_namespace (ns_child.second, depth+1);
    }
}


void
traverse (tree ns)
{
    decl_set set;
    collect (ns, set);
    std::cerr << "done with collect" << std::endl;

    for (decl_set::iterator i (set.begin ()), e (set.end ());
         i != e; ++i)
    {
        print_decl (*i, &g_namespace);
    }

    dump_namespace (g_namespace, 0);
}

extern "C" void
gate_callback (void*, void*)
{
    // If there were errors during compilation,
    // let GCC handle the exit.
    //
    if (errorcount || sorrycount)
        return;

    int r (0);

    //
    // Process AST. Issue diagnostics and set r
    // to 1 in case of an error.
    //
    std::cerr << "processing " << main_input_filename << std::endl;
    traverse (global_namespace);
    exit (r);
}

extern "C" int
plugin_init (plugin_name_args* info,
             plugin_gcc_version* ver)
{
    int r (0);

    std::cerr << "starting " << info->base_name << std::endl;

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
        std::cerr << "unable to open output file" << std::endl;
        r = -1;
    }

    if (r == 0)
    {
        // Disable assembly output.
        //
        asm_file_name = HOST_BIT_BUCKET;

        // Register callbacks.
        //
        register_callback (info->base_name,
                           PLUGIN_PRE_GENERICIZE,
                           &gate_callback,
                           0);
    }
    return r;
}

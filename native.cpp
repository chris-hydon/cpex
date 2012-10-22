/// Some functions to initalise the library
extern "C" {

// These seem to be necessary to make the following includes work
typedef void* StgFunTable;
typedef void* StgRegTable;

#include <HsFFI.h>
#include <RtsAPI.h>

// Haskell includes
#include <CSPM/Foreign_stub.h>
#include <Cpex/Transitions_stub.h>
}

namespace CSPM
{

/// Initalise the library with the specified options. This function modifies
/// them to remove any Haskell arguments.
void library_init(int* argc, char*** argv)
{
    RtsConfig conf = defaultRtsConfig;
    conf.rts_opts_enabled = RtsOptsAll;
    hs_init_ghc(argc, argv, conf);
}

void library_exit()
{
    hs_exit();
}

}


// Misc includes
#include <iostream>
#include <stdlib.h>

int main(int argc, char **argv)
{
    CSPM::library_init(&argc, &argv);

    std::cout << "Library init succeeded" << std::endl;

    std::cout << "Loading " << argv[1] << std::endl;
    // Load a file
    void* session = cspm_session_create();
    void* file = NULL;
    // All strings args need to be wchar_t strings
    std::string fname(argv[1]);
    std::wstring file_name(fname.begin(), fname.end());
    if (!cspm_session_load_file(session, (void*) file_name.c_str(), &file))
    {
        std::cout << "Could not load " << argv[1] << std::endl;

        wchar_t** errors = NULL;
        unsigned int error_count;
        cspm_session_get_errors(session, &errors, &error_count);
        cspm_session_clear_errors(session);
        std::cout << "There were " << error_count << " errors" << std::endl;
        for (unsigned int i = 0; i < error_count; ++i)
        {
            std::wcout << std::wstring(errors[i]) << std::endl;
            free(errors[i]);
        }
        free(errors);

        cspm_session_free(session);
        return 1;
    }

    // Succeeded - but there may have been warnings
    wchar_t** warnings = NULL;
    unsigned int warning_count;
    cspm_session_get_warnings(session, &warnings, &warning_count);
    cspm_session_clear_warnings(session);
    for (unsigned int i = 0; i < warning_count; ++i)
    {
        std::wcout << std::wstring(warnings[i]) << std::endl;
        free(warnings[i]);
    }
    free(warnings);

    std::cout << "File loaded" << std::endl;

    std::cout << "The following names are bound:" << std::endl;
    wchar_t** names = NULL;
    unsigned int count = 0;
    unsigned int r = cspm_session_bound_names(session, &names, &count);
    if (r == 0) return NULL;
    for (unsigned int i = 0; i < count; ++i)
    {
        std::wcout << "  " << std::wstring(names[i]) << std::endl;
        free(names[i]);
    }
    free(names);

    std::string expr = argv[2];
    std::wstring expression(expr.begin(), expr.end());
    std::wcout << "Evaluating " << expression << std::endl;
    void* interactive_stmt = NULL;
    if (!cspm_session_parse_interactive_stmt(session, (void*) expression.c_str(),
            &interactive_stmt))
    {
        // HANDLE ERRORS
        std::cout << "error\n";
        return 1;
    }

    // HANDLE WARNINGS

    // Check the type is an expression using cspm_interactive_stmt_type

    wchar_t* result_str = NULL;
    if (!cspm_interactive_stmt_evaluate(session, interactive_stmt, &result_str))
    {
        // HANDLE ERRORS
    }

    std::wcout << "Result of " << expression << " is " <<
        std::wstring(result_str) << std::endl;
    free(result_str);

    std::string exp2 = argv[3];
    std::wstring expression2(exp2.begin(), exp2.end());
    std::wcout << "Finding transitions of " << expression2 << std::endl;

    void* proc = NULL;
    if (!cpex_expression_value(session, (void *) expression2.c_str(), &proc))
    {
      // HANDLE ERRORS
      std::cout << "error\n";
      return 1;
    }

    void** transitions = NULL;
    unsigned int transitionCount = 0;
    cpex_transitions(proc, &transitions, &transitionCount);

    for (int i = 0; i < transitionCount; i++)
    {
      wchar_t* name = NULL;
      unsigned int type = 0;
      cpex_event_data(transitions[i], &name, &type);
      switch (type)
      {
        case 0:
          std::wcout << "Event " << i << ": " << name << std::endl;
          break;
        case 1:
          std::cout << "Event " << i << ": τ" << std::endl;
          break;
        case 2:
          std::cout << "Event " << i << ": ✓" << std::endl;
          break;
      }
    }

    cspm_interactive_stmt_free(interactive_stmt);
    cspm_file_free(file);
    cspm_session_free(session);

    CSPM::library_exit();
    return 0;
}

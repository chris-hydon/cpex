#include "cspmsession.h"

#include "haskell/CSPM/Foreign_stub.h"
#include "haskell/Cpex/Transitions_stub.h"
#include <QStringList>

CSPMSession * CSPMSession::_session = NULL;

void CSPMSession::freeSession()
{
  if (CSPMSession::_session != NULL)
  {
    delete CSPMSession::_session;
    CSPMSession::_session = NULL;
  }
}

CSPMSession * CSPMSession::getSession()
{
  if (CSPMSession::_session == NULL)
  {
    CSPMSession::_session = new CSPMSession();
  }

  return CSPMSession::_session;
}

CSPMSession::CSPMSession()
{
  _hsSession = cspm_session_create();
  _file = NULL;
}

CSPMSession::~CSPMSession()
{
  if (_file != NULL)
  {
    cspm_file_free(_file);
  }
  cspm_session_free(_hsSession);
}

int CSPMSession::loadFile(QString fileName)
{
  if (_file != NULL)
  {
    cspm_file_free(_file);
  }
  return cspm_session_load_file(_hsSession, (void *) fileName.toStdWString().c_str(), &_file);
}

Process * CSPMSession::compileExpression(QString expression)
{
  void * proc = NULL;
  int r = cpex_expression_value(_hsSession, (void *) expression.toStdWString().c_str(), &proc);
  return (r ? new Process(proc) : NULL);
}

QStringList CSPMSession::procCallNames()
{
  wchar_t ** strs = NULL;
  quint32 * lens = NULL;
  quint32 count = 0;
  if (!cpex_proccall_names(_hsSession, &strs, &lens, &count))
  {
    return QStringList();
  }

  QStringList strings;
  for (quint32 i = 0; i < count; i++)
  {
    QString params = "";
    if (lens[i] > 0)
    {
      params = "(";
      for (quint32 j = 0; j < lens[i]; j++)
      {
        if (j + 1 == lens[i])
        {
          params += "_)";
        }
        else
        {
          params += "_, ";
        }
      }
    }
    strings << QString::fromWCharArray(strs[i]) + params;
  }

  free(strs);
  free(lens);
  return strings;
}

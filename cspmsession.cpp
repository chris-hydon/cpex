#include "cspmsession.h"

#include "haskell/CSPM/Foreign_stub.h"
#include "haskell/Cpex/Foreign_stub.h"
#include <QStringList>

CSPMSession::CSPMSession()
{
  _hsSession = cspm_session_create();
  _file = NULL;
  _procCallNames = QStringList();
  _procCallNamesLoaded = true;
}

CSPMSession::~CSPMSession()
{
  if (_file != NULL)
  {
    cspm_file_free(_file);
  }
  cspm_session_free(_hsSession);
}

const QString & CSPMSession::fileName() const
{
  return _fileName;
}

int CSPMSession::loadFile(const QString & fileName)
{
  _fileName = fileName;
  _procCallNames = QStringList();
  _procCallNamesLoaded = false;
  if (_file != NULL)
  {
    cspm_file_free(_file);
  }
  return cspm_session_load_file(_hsSession, (void *) fileName.toStdWString().c_str(), &_file);
}

Process CSPMSession::compileExpression(const QString & expression) const
{
  void * proc = NULL;
  int r = cpex_expression_value(_hsSession, (void *) expression.toStdWString().c_str(), &proc);
  return (r ? Process(proc) : Process());
}

QStringList CSPMSession::procCallNames() const
{
  if (!_procCallNamesLoaded)
  {
    _procCallNamesLoaded = true;
    wchar_t ** strs = NULL;
    quint32 * lens = NULL;
    quint32 count = 0;
    if (!cpex_proccall_names(_hsSession, &strs, &lens, &count))
    {
      return QStringList();
    }

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
      _procCallNames << QString::fromWCharArray(strs[i]) + params;
    }

    free(strs);
    free(lens);
  }

  return _procCallNames;
}

bool CSPMSession::operator ==(const CSPMSession & other)
{
  // Two sessions are equal if they point to the same Haskell session.
  return other._hsSession == _hsSession;
}

#include "cspmsession.h"

#include "haskell/CSPM/Foreign_stub.h"
#include "haskell/Cpex/Foreign_stub.h"
#include <QFileInfo>
#include <QStringList>
#include "programstate.h"

CSPMSession::CSPMSession() : _procs(new QSet<Process>)
{
  _hsSession = cspm_session_create();
  _file = NULL;
  _procCallNames = QStringList();
  _procCallNamesLoaded = true;

  // This is likely to get very big.
  _procs->reserve(20000);
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

const QString & CSPMSession::displayName() const
{
  return _displayName;
}

bool CSPMSession::loadFile(const QString & fileName)
{
  void * file;
  if (cspm_session_load_file(_hsSession, (void *) fileName.toStdWString().c_str(), &file))
  {
    if (_file != NULL)
    {
      cspm_file_free(_file);
    }
    _file = file;
    _fileName = fileName;
    _procCallNames = QStringList();
    _procCallNamesLoaded = false;
    _procs->clear();

    QString disp = QFileInfo(_fileName).baseName().replace(QRegExp("[^0-9a-z]"), "");
    _displayName = disp;
    int i = 0;
    QMap<QString, CSPMSession *> sessions = ProgramState::getSessions();
    while (sessions.contains(_displayName))
    {
      _displayName = disp + QString::number(i++);
    }
    return true;
  }
  return false;
}

Process CSPMSession::compileExpression(const QString & expression) const
{
  void * proc = NULL;
  int r = cpex_expression_value(_hsSession,
    (void *) expression.toStdWString().c_str(), &proc);
  return (r ? Process::create(proc, this) : Process());
}

Event CSPMSession::stringToEvent(const QString & expression) const
{
  void * event = NULL;
  int r = cpex_string_to_event(_hsSession,
    (void *) expression.toStdWString().c_str(), &event);
  return (r ? Event(event) : Event());
}

QStringList CSPMSession::getErrors() const
{
  wchar_t ** errors = NULL;
  quint32 count = 0;
  cspm_session_get_errors(_hsSession, &errors, &count);
  QStringList ret;
  for (quint32 i = 0; i < count; i++)
  {
    ret << QString::fromWCharArray(errors[i]);
  }

  free(errors);
  cspm_session_clear_errors(_hsSession);
  return ret;
}

QStringList CSPMSession::getWarnings() const
{
  wchar_t ** warns = NULL;
  quint32 count = 0;
  cspm_session_get_warnings(_hsSession, &warns, &count);
  QStringList ret;
  for (quint32 i = 0; i < count; i++)
  {
    ret << QString::fromWCharArray(warns[i]);
  }

  free(warns);
  cspm_session_clear_warnings(_hsSession);
  return ret;
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

QSet<Process> * CSPMSession::procs() const
{
  return _procs;
}

void * CSPMSession::getHsPtr() const
{
  return _hsSession;
}

bool CSPMSession::operator ==(const CSPMSession & other) const
{
  // Two sessions are equal if they point to the same Haskell session.
  return other._hsSession == _hsSession;
}

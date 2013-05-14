#include "cspmsession.h"

#include "haskell/CSPM/Foreign_stub.h"
#include "haskell/Cpex/Foreign_stub.h"
#include <QFileInfo>
#include <QStringList>
#include "programstate.h"

CSPMSession::CSPMSession() : _procs(new QSet<Process>),
  _events(new QHash<size_t, Event>)
{
  _hsSession = cspm_session_create();
  _file = NULL;
  _procCallNames = QStringList();
  _procCallNamesLoaded = true;
  _displayName = "(default)";

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
    _events->clear();

    QString disp = QFileInfo(_fileName).baseName().replace(QRegExp("[^0-9a-z]"), "");
    _displayName = disp;
    int i = 0;
    QMap<QString, CSPMSession *> sessions = ProgramState::get()->getSessions();
    while (sessions.contains(_displayName))
    {
      _displayName = disp + QString::number(i++);
    }
    getWarnings();
    return true;
  }
  else
  {
    getWarnings();
    getErrors();
    return false;
  }
}

bool CSPMSession::reload()
{
  if (_file != NULL)
  {
    void * sess = cspm_session_create();
    void * file;
    if (cspm_session_load_file(sess, (void *) _fileName.toStdWString().c_str(), &file))
    {
      cspm_file_free(_file);
      cspm_session_free(_hsSession);
      _file = file;
      _hsSession = sess;
      _procCallNames = QStringList();
      _procCallNamesLoaded = false;
      _procs->clear();
      _events->clear();
      getWarnings();
      return true;
    }
    else
    {
      // To avoid repeating code, briefly change _hsSession.
      void * temp = _hsSession;
      _hsSession = sess;
      getWarnings();
      getErrors();
      _hsSession = temp;
      cspm_session_free(sess);
      return false;
    }
  }

  // No file, so this is a blank session - we don't add anything so there's nothing
  // to change, nothing to do.
  return true;
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
  return (r ? Event::create(this, event) : Event());
}

QStringList CSPMSession::getErrors() const
{
  wchar_t ** errors = NULL;
  quint32 count = 0;
  cspm_session_get_errors(_hsSession, &errors, &count);
  QStringList ret;
  QString message;
  for (quint32 i = 0; i < count; i++)
  {
    message = QString::fromWCharArray(errors[i]);
    ret << message;
    ProgramState::get()->logError(new CSPError(this, CSPError::Error, message));
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
  QString message;
  for (quint32 i = 0; i < count; i++)
  {
    message = QString::fromWCharArray(warns[i]);
    ret << message;
    ProgramState::get()->logError(new CSPError(this, CSPError::Warning, message));
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
    wchar_t ** args = NULL;
    quint32 count = 0;
    if (!cpex_proccall_names(_hsSession, &strs, &args, &count))
    {
      return QStringList();
    }

    for (quint32 i = 0; i < count; i++)
    {
      _procCallNames << QString::fromWCharArray(strs[i]) +
        QString::fromWCharArray(args[i]);
    }

    free(strs);
    free(args);
  }

  return _procCallNames;
}

QSet<Process> * CSPMSession::procs() const
{
  return _procs;
}

QHash<size_t, Event> * CSPMSession::events() const
{
  return _events;
}

void * CSPMSession::getHsPtr() const
{
  return _hsSession;
}

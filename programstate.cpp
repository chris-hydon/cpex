#include "programstate.h"

ProgramState ProgramState::state;

ProgramState::ProgramState(QObject * parent) : QObject(parent),
  _sessions(QMap<QString, CSPMSession *>()), _currentSession(NULL),
  _blankSession(NULL), _errors(QList<CSPError *>())
{
}

ProgramState * ProgramState::get()
{
  return &state;
}

QMap<QString, CSPMSession *> ProgramState::getSessions()
{
  return _sessions;
}

CSPMSession * ProgramState::newSession(const QString & fileName)
{
  CSPMSession * session = new CSPMSession();
  if (session->loadFile(fileName))
  {
    _sessions.insert(session->displayName(), session);
    return session;
  }
  else
  {
    delete session;
    return NULL;
  }
}

void ProgramState::deleteSession(CSPMSession * session)
{
  // Do nothing if this is the blank session - it will never change and ought to
  // always exist.
  if (session == _blankSession)
  {
    return;
  }

  // Nullify the current session if it is the one we're removing.
  if (session == _currentSession)
  {
    _currentSession = NULL;
  }

  // Delete it and remove it from the list of sessions.
  _sessions.remove(session->displayName());
  delete session;
}

CSPMSession * ProgramState::currentSession()
{
  if (_currentSession != NULL)
  {
    return _currentSession;
  }

  if (_blankSession == NULL)
  {
    _blankSession = new CSPMSession();
  }
  return _blankSession;
}

void ProgramState::setCurrentSession(CSPMSession * session)
{
  _currentSession = session;
}

QList<CSPError *> ProgramState::getErrors()
{
  return _errors;
}

void ProgramState::logError(CSPError * error)
{
  _errors.append(error);
  emit errorCountChanged(_errors.length());
}

void ProgramState::deleteErrors(const QList<int> & indices)
{
  // Delete from highest to lowest, otherwise this is unsafe due to takeAt.
  QList<int> toDelete = indices;
  qSort(toDelete.begin(), toDelete.end(), qGreater<int>());
  int count = toDelete.count();
  for (int i = 0; i < count; i++)
  {
    delete _errors.takeAt(toDelete[i]);
  }
  emit errorCountChanged(_errors.length());
}

void ProgramState::cleanup()
{
  QList<CSPMSession *> toDelete = _sessions.values();
  while (!toDelete.isEmpty())
  {
    delete toDelete.takeFirst();
  }

  while (!_errors.isEmpty())
  {
    delete _errors.takeFirst();
  }

  if (_blankSession != NULL)
  {
    delete _blankSession;
    _blankSession = NULL;
  }
}

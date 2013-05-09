#include "programstate.h"
#include "mainwindow.h"

QMap<QString, CSPMSession *> ProgramState::_sessions;
CSPMSession * ProgramState::_currentSession = NULL;
CSPMSession * ProgramState::_blankSession = NULL;
QList<CSPError *> ProgramState::_errors;

QMap<QString, CSPMSession *> ProgramState::getSessions()
{
  return ProgramState::_sessions;
}

CSPMSession * ProgramState::newSession(const QString & fileName)
{
  CSPMSession * session = new CSPMSession();
  if (session->loadFile(fileName))
  {
    ProgramState::_sessions.insert(session->displayName(), session);
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
  ProgramState::_sessions.remove(session->displayName());
  delete session;
}

CSPMSession * ProgramState::currentSession()
{
  if (_currentSession != NULL)
  {
    return ProgramState::_currentSession;
  }

  if (_blankSession == NULL)
  {
    _blankSession = new CSPMSession();
  }
  return _blankSession;
}

void ProgramState::setCurrentSession(CSPMSession * session)
{
  ProgramState::_currentSession = session;
}

QList<CSPError *> ProgramState::getErrors()
{
  return _errors;
}

void ProgramState::logError(CSPError * error)
{
  _errors.append(error);
  MainWindow::get()->setErrorCount(_errors.length());
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
  MainWindow::get()->setErrorCount(_errors.length());
}

void ProgramState::cleanup()
{
  QList<CSPMSession *> toDelete = ProgramState::_sessions.values();
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

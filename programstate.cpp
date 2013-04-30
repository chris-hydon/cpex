#include "programstate.h"

QMap<QString, CSPMSession *> ProgramState::_sessions;
CSPMSession * ProgramState::_currentSession = NULL;
CSPMSession * ProgramState::_blankSession = NULL;

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

void ProgramState::cleanup()
{
  CSPMSession * session;
  QList<CSPMSession *> toDelete = ProgramState::_sessions.values();
  while (!toDelete.isEmpty())
  {
    session = toDelete.takeFirst();
    delete session;
  }

  if (_blankSession != NULL)
  {
    delete _blankSession;
    _blankSession = NULL;
  }
}

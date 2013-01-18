#include "programstate.h"

QList<CSPMSession *> ProgramState::_sessions;
CSPMSession * ProgramState::_currentSession = NULL;
CSPMSession * ProgramState::_blankSession = NULL;

QList<CSPMSession *> ProgramState::getSessions()
{
  return ProgramState::_sessions;
}

CSPMSession * ProgramState::newSession(const QString & fileName)
{
  CSPMSession * session = new CSPMSession();
  if (session->loadFile(fileName))
  {
    ProgramState::_sessions.append(session);
    ProgramState::_currentSession = session;
    return session;
  }
  else
  {
    delete session;
    return NULL;
  }
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
  while (!ProgramState::_sessions.isEmpty())
  {
    session = ProgramState::_sessions.takeFirst();
    delete session;
  }

  if (_blankSession != NULL)
  {
    delete _blankSession;
    _blankSession = NULL;
  }
}

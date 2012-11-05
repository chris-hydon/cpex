#include "programstate.h"

QList<CSPMSession *> ProgramState::_sessions;
CSPMSession * ProgramState::_currentSession;

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
  return ProgramState::_currentSession;
}

void ProgramState::setCurrentSession(CSPMSession * session)
{
  ProgramState::_currentSession = session;
}

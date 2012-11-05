#ifndef PROGRAMSTATE_H
#define PROGRAMSTATE_H

#include "cspmsession.h"
#include <QList>

class ProgramState
{
public:
  static QList<CSPMSession *> getSessions();
  static CSPMSession * newSession(const QString & fileName);
  static CSPMSession * currentSession();
  static void setCurrentSession(CSPMSession * session);

private:
  static QList<CSPMSession *> _sessions;
  static CSPMSession * _currentSession;
};

#endif // PROGRAMSTATE_H

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
  static CSPMSession * blankSession();
  static void setCurrentSession(CSPMSession * session);
  static void cleanup();

private:
  static QList<CSPMSession *> _sessions;
  static CSPMSession * _currentSession;
  static CSPMSession * _blankSession;
};

#endif // PROGRAMSTATE_H

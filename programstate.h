#ifndef PROGRAMSTATE_H
#define PROGRAMSTATE_H

#include "cspmsession.h"
#include "csperror.h"
#include <QMap>

class ProgramState
{
public:
  static QMap<QString, CSPMSession *> getSessions();
  static CSPMSession * newSession(const QString & fileName);
  static void deleteSession(CSPMSession * session);
  static CSPMSession * currentSession();
  static CSPMSession * blankSession();
  static void setCurrentSession(CSPMSession * session);
  static QList<CSPError *> getErrors();
  static void logError(CSPError *);
  static void deleteErrors(const QList<int> &);
  static void cleanup();

private:
  static QMap<QString, CSPMSession *> _sessions;
  static CSPMSession * _currentSession;
  static CSPMSession * _blankSession;
  static QList<CSPError *> _errors;
};

#endif // PROGRAMSTATE_H

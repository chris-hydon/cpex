#ifndef PROGRAMSTATE_H
#define PROGRAMSTATE_H

#include "cspmsession.h"
#include "csperror.h"
#include <QMap>

class ProgramState : public QObject
{
  Q_OBJECT

public:
  virtual ~ProgramState() {}
  static ProgramState * get();
  QMap<QString, CSPMSession *> getSessions();
  CSPMSession * newSession(const QString & fileName);
  void deleteSession(CSPMSession * session);
  CSPMSession * currentSession();
  CSPMSession * blankSession();
  void setCurrentSession(CSPMSession * session);
  QList<CSPError *> getErrors();
  void logError(CSPError *);
  void deleteErrors(const QList<int> &);
  void cleanup();

signals:
  void errorCountChanged(int);

private:
  ProgramState(QObject * = 0);
  static ProgramState state;
  QMap<QString, CSPMSession *> _sessions;
  CSPMSession * _currentSession;
  CSPMSession * _blankSession;
  QList<CSPError *> _errors;
};

#endif // PROGRAMSTATE_H

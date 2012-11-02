#ifndef CSPMSESSION_H
#define CSPMSESSION_H

#include "model/process.h"
#include <QString>

class CSPMSession
{
public:
  static CSPMSession * getSession();
  static void freeSession();

  int loadFile(QString fileName);
  Process * compileExpression(QString expression);
  QStringList procCallNames();

private:
  static CSPMSession * _session;

  CSPMSession();
  ~CSPMSession();
  void * _hsSession;
  void * _file;
};

#endif // CSPMSESSION_H

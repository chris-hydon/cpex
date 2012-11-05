#ifndef CSPMSESSION_H
#define CSPMSESSION_H

#include "model/process.h"
#include <QString>
#include <QStringList>

class CSPMSession
{
public:
  CSPMSession();
  ~CSPMSession();
  const QString & fileName() const;
  int loadFile(const QString & fileName);
  Process * compileExpression(const QString & expression);
  QStringList procCallNames() const;

private:
  void * _hsSession;
  void * _file;
  QString _fileName;
  mutable QStringList _procCallNames;
  mutable bool _procCallNamesLoaded;
};

#endif // CSPMSESSION_H

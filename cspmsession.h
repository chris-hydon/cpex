#ifndef CSPMSESSION_H
#define CSPMSESSION_H

#include "model/process.h"
#include <QSet>
#include <QString>
#include <QStringList>

class CSPMSession
{
public:
  CSPMSession();
  ~CSPMSession();
  const QString & fileName() const;
  int loadFile(const QString & fileName);
  Process compileExpression(const QString & expression) const;
  QStringList procCallNames() const;
  QSet<Process> * procs() const;
  void * getHsPtr() const;
  bool operator ==(const CSPMSession & other);

private:
  void * _hsSession;
  void * _file;
  QString _fileName;
  QSet<Process> * const _procs;
  mutable QStringList _procCallNames;
  mutable bool _procCallNamesLoaded;
};

#endif // CSPMSESSION_H

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
  const QString & displayName() const;
  bool loadFile(const QString & fileName);
  Process compileExpression(const QString & expression) const;
  Event stringToEvent(const QString & expression) const;
  QStringList getErrors() const;
  QStringList getWarnings() const;
  QStringList procCallNames() const;
  QSet<Process> * procs() const;
  QHash<size_t, Event> * events() const;
  void * getHsPtr() const;
  bool operator ==(const CSPMSession & other) const;

private:
  void * _hsSession;
  void * _file;
  QString _fileName;
  QString _displayName;
  QSet<Process> * _procs;
  // A hash of events, keyed by the StablePtr created by Haskell. The size of this
  // pointer varies depending on architecture, hence the use of size_t.
  QHash<size_t, Event> * _events;
  mutable QStringList _procCallNames;
  mutable bool _procCallNamesLoaded;
};

#endif // CSPMSESSION_H

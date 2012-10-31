#ifndef PROCESS_H
#define PROCESS_H

#include "event.h"

#include <QList>
#include <QPair>
#include <QString>

class Process
{
public:
  Process(void * hsPtr, const Process * parent = NULL, const Event * cause = NULL,
    int index = 0);
  ~Process();
  QList<QPair<Event *, Process *> *> * transitions() const;
  QString displayText() const;
  const Process * parent() const;
  const Event * causedBy() const;
  int parentTransitionIndex() const;
  bool operator ==(const Process & other) const;

private:
  void * _hsPtr;
  const Process * _parent;
  const Event * _cause;
  int _index;
  mutable QList<QPair<Event *, Process *> *> * _next;
  mutable QString _displayText;
};

#endif // PROCESS_H

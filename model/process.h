#ifndef PROCESS_H
#define PROCESS_H

#include "event.h"

#include <QList>
#include <QPair>
#include <QSharedData>
#include <QString>

class ProcessData;

class Process
{
public:
  Process(void * hsPtr);
  Process(const Process & other);
  ~Process();
  QList<QPair<Event *, Process *> *> * transitions() const;
  const Process * findEqual(const Process * to) const;
  QString displayText() const;
  bool operator ==(const Process & other) const;

private:
  Process(void * hsPtr, const Process * parent);
  QExplicitlySharedDataPointer<ProcessData> _d;
};

#endif // PROCESS_H

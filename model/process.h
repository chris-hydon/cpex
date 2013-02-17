#ifndef PROCESS_H
#define PROCESS_H

#include "event.h"
#include "view/displaystring.h"

#include <QList>
#include <QPair>
#include <QSharedData>
#include <QString>

class CSPMSession;

class ProcessData;

class Process
{
public:
  Process();
  Process(const Process & other);
  ~Process();
  QList<QPair<Event, Process> > transitions() const;
  QList<Process> components(bool expandCall = false) const;
  bool offersEvent(Event) const;
  DisplayString displayText() const;
  QString fullText() const;
  QString toolTip() const;
  QString whyEvent(const Event &) const;
  bool isValid() const;
  const CSPMSession * session() const;
  bool operator ==(const Process & other) const;
  uint hash() const;
  const Process & operator =(const Process & other);

  static Process create(void * hsPtr, const CSPMSession * session);

private:
  Process(void * hsPtr, const CSPMSession * session);
  QExplicitlySharedDataPointer<ProcessData> _d;
};

inline uint qHash(const Process & p)
{
  return p.hash();
}

#endif // PROCESS_H

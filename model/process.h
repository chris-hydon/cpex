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
  QList<QPair<Event, Process> > transitions(bool) const;
  QList<Process> components(bool expandCall = false) const;
  bool offersEvent(bool, Event) const;
  QList<Event> offeredEvents(bool, const QList<Event> & = QList<Event>()) const;
  DisplayString displayText() const;
  QString fullText() const;
  QString toolTip() const;
  QString whyEvent(const QList<Event> &, bool, bool) const;
  QHash<int, QList<Event> > eventsRequiredBySuccessors(const QList<Event> &, bool)
    const;
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

#include "process.h"

#include "haskell/Cpex/Foreign_stub.h"
#include <HsFFI.h>
#include <QHash>
#include <QRegExp>
#include <QSharedData>

class ProcessData : public QSharedData
{
public:
  ProcessData(void * hsPtr) : hsPtr(hsPtr), isRoot(true), loaded(false)
  {
    states = new QHash<QString, const Process *>;
  }

  ProcessData(void * hsPtr, QHash<QString, const Process *> * states) : hsPtr(hsPtr),
    isRoot(false), loaded(false), states(states)
  {
  }

  ProcessData(const ProcessData & other) : QSharedData(other), hsPtr(other.hsPtr),
    isRoot(false), loaded(true), next(other.next), displayText(other.displayText)
  {
  }

  ~ProcessData()
  {
    QPair<Event, Process *> deleting;
    while (!next.isEmpty())
    {
      delete next.takeFirst().second;
    }

    if (isRoot)
    {
      delete states;
    }

    hs_free_stable_ptr(hsPtr);
  }

  void * hsPtr;
  const bool isRoot;
  mutable bool loaded;
  mutable QList<QPair<Event, Process *> > next;
  mutable QString displayText;
  mutable QHash<QString, const Process *> * states;
};

Process::Process(const Process & other) : _d(other._d)
{
}

Process::Process(void * hsPtr)
{
  _d = new ProcessData(hsPtr);
}

Process::Process(void * hsPtr, const Process * parent)
{
  _d = new ProcessData(hsPtr, parent->_d->states);
}

Process::~Process()
{
}

QList<QPair<Event, Process *> > Process::transitions() const
{
  if (!_d->loaded)
  {
    void ** hsProcs = NULL;
    void ** hsEvents = NULL;
    quint32 transitionCount = 0;
    cpex_transitions(_d->hsPtr, &hsEvents, &hsProcs, &transitionCount);

    for (quint32 i = 0; i < transitionCount; i++)
    {
      Event e(hsEvents[i]);
      Process * p = new Process(hsProcs[i], this);
      const Process * pExists = findEqual(p);
      if (pExists != NULL)
      {
        p->_d = pExists->_d;
      }
      _d->next.append(QPair<Event, Process *>(e, p));
    }

    free(hsProcs);
    free(hsEvents);
    _d->loaded = true;
  }

  return QList<QPair<Event, Process *> >(_d->next);
}

const Process * Process::findEqual(const Process *) const
{
  // Removed - should be able to do better, right now this is slow for states
  // represented by large strings and might not necessarily match identical
  // states (since most operators are associative).
  return NULL;
/*
  // Look for "to" in the list of states in this machine.
  const Process * found = _d->states->value(to->displayText(), NULL);
  if (found == NULL)
  {
    _d->states->insert(to->displayText(), to);
  }
  return found;
*/
}

QString Process::displayText() const
{
  if (_d->displayText == QString())
  {
    wchar_t * str = NULL;
    cpex_process_string(_d->hsPtr, &str);
    _d->displayText = QString::fromWCharArray(str).replace(QRegExp("\\s+"), " ");
    free(str);
  }
  return _d->displayText;
}

bool Process::operator ==(const Process & other) const
{
  // Equate two processes if they use the same data.
  return _d == other._d;
}

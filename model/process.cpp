#include "process.h"

#include "haskell/Cpex/Foreign_stub.h"
#include <HsFFI.h>
#include <QHash>
#include <QRegExp>
#include <QSharedData>

class ProcessData : public QSharedData
{
public:
  ProcessData(void * hsPtr) : hsPtr(hsPtr), isRoot(true), next(NULL)
  {
    states = new QHash<QString, const Process *>;
  }

  ProcessData(void * hsPtr, QHash<QString, const Process *> * states) : hsPtr(hsPtr),
    isRoot(false), next(NULL), states(states)
  {
  }

  ProcessData(const ProcessData & other) : QSharedData(other), hsPtr(other.hsPtr),
    isRoot(false), next(other.next), displayText(other.displayText)
  {
  }

  ~ProcessData()
  {
    QPair<Event *, Process *> * deleting;
    if (next != NULL)
    {
      while (!next->isEmpty())
      {
        deleting = next->takeFirst();
        delete deleting->first;
        delete deleting->second;
        delete deleting;
      }
      delete next;
    }

    if (isRoot)
    {
      delete states;
    }

    hs_free_stable_ptr(hsPtr);
  }

  void * hsPtr;
  const bool isRoot;
  mutable QList<QPair<Event *, Process *> *> * next;
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

QList<QPair<Event *, Process *> *> * Process::transitions() const
{
  if (_d->next == NULL)
  {
    void ** hsProcs = NULL;
    void ** hsEvents = NULL;
    quint32 transitionCount = 0;
    cpex_transitions(_d->hsPtr, &hsEvents, &hsProcs, &transitionCount);

    _d->next = new QList<QPair<Event *, Process *> *>();
    for (quint32 i = 0; i < transitionCount; i++)
    {
      Event * e = new Event(hsEvents[i]);
      Process * p = new Process(hsProcs[i], this);
      const Process * pExists = findEqual(p);
      if (pExists != NULL)
      {
        p->_d = pExists->_d;
      }
      _d->next->append(new QPair<Event *, Process *>(e, p));
    }

    free(hsProcs);
    free(hsEvents);
  }

  return _d->next;
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

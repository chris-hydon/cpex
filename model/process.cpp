#include "process.h"

#include "haskell/Cpex/Transitions_stub.h"
#include <HsFFI.h>
#include <QRegExp>

Process::Process(void * hsPtr, const Process * parent, const Event * cause,
  int index) : _hsPtr(hsPtr), _parent(parent), _cause(cause), _index(index)
{
  _next = NULL;
  _displayText = QString();
}

Process::~Process()
{
  QPair<Event *, Process *> * deleting;
  if (_next != NULL)
  {
    while (!_next->isEmpty())
    {
      deleting = _next->takeFirst();
      delete deleting->first;
      delete deleting->second;
      delete deleting;
    }
    delete _next;
  }
  hs_free_stable_ptr(_hsPtr);
}

QList<QPair<Event *, Process *> *> * Process::transitions() const
{
  if (_next == NULL)
  {
    void ** hsProcs = NULL;
    void ** hsEvents = NULL;
    quint32 transitionCount = 0;
    cpex_transitions(_hsPtr, &hsEvents, &hsProcs, &transitionCount);

    _next = new QList<QPair<Event *, Process *> *>();
    for (quint32 i = 0; i < transitionCount; i++)
    {
      Event * e = new Event(hsEvents[i]);
      Process * p = new Process(hsProcs[i], this, e, i);
      _next->append(new QPair<Event *, Process *>(e, p));
    }

    free(hsProcs);
    free(hsEvents);
  }

  return _next;
}

QString Process::displayText() const
{
  if (_displayText == QString())
  {
    wchar_t * str = NULL;
    cpex_process_string(_hsPtr, &str);
    _displayText = QString::fromWCharArray(str).replace(QRegExp("\\s+"), " ");
    free(str);
  }
  return _displayText;
}

const Process * Process::parent() const
{
  return _parent;
}

const Event * Process::causedBy() const
{
  return _cause;
}

int Process::parentTransitionIndex() const
{
  return _index;
}

bool Process::operator ==(const Process & other) const
{
  return displayText() == other.displayText();
}

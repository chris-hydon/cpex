#include "process.h"

#include "haskell/Cpex/Foreign_stub.h"
#include "model/ptypes.h"
#include "cspmsession.h"

#include <HsFFI.h>
#include <QHash>
#include <QRegExp>
#include <QSet>
#include <QSharedData>

class ProcessData : public QSharedData
{
public:
  ProcessData(void * hsPtr, const CSPMSession * session) : session(session),
    hsPtr(hsPtr), loaded(false)
  {
  }

  ProcessData(const ProcessData & other) : QSharedData(other),
    session(other.session), hsPtr(other.hsPtr), backend(other.backend),
    loaded(true), next(other.next), displayText(other.displayText)
  {
  }

  ~ProcessData()
  {
    delete backend;
    hs_free_stable_ptr(hsPtr);
  }

  const CSPMSession * session;
  void * hsPtr;
  PBase * backend;
  mutable bool loaded;
  mutable QList<QPair<Event, Process> > next;
  mutable DisplayString displayText;
  mutable QString fullText;
};

Process::Process()
{
}

Process::Process(const Process & other) : _d(other._d)
{
}

Process::Process(void * hsPtr, const CSPMSession * session)
{
  _d = new ProcessData(hsPtr, session);
  unsigned char type = 0;
  cpex_process_operator(hsPtr, &type);

  // Choose which backend class to use - this will allow examination of the
  // structure of the processes.
  switch(type)
  {
    case 0:
      _d->backend = new PAlphaParallel(hsPtr, session);
      break;
    case 1:
      _d->backend = new PException(hsPtr, session);
      break;
    case 2:
      _d->backend = new PExternalChoice(hsPtr, session);
      break;
    case 3:
      _d->backend = new PGenParallel(hsPtr, session);
      break;
    case 4:
      _d->backend = new PHide(hsPtr, session);
      break;
    case 5:
      _d->backend = new PInternalChoice(hsPtr, session);
      break;
    case 6:
      _d->backend = new PInterrupt(hsPtr, session);
      break;
    case 7:
      _d->backend = new PInterleave(hsPtr, session);
      break;
    case 8:
      _d->backend = new PLinkParallel(hsPtr, session);
      break;
    case 9:
      _d->backend = new POperator(hsPtr, session);
      break;
    case 10:
      _d->backend = new PPrefix(hsPtr, session);
      break;
    case 11:
      _d->backend = new PRename(hsPtr, session);
      break;
    case 12:
      _d->backend = new PSequentialComp(hsPtr, session);
      break;
    case 13:
      _d->backend = new PSlidingChoice(hsPtr, session);
      break;
    case 14:
      _d->backend = new PProcCall(hsPtr, session);
      break;
  }
}

Process Process::create(void * hsPtr, const CSPMSession * session)
{
  // Create a new one, then see if it already exists. Behind the scenes, this
  // ends up building the complete process.
  Process p(hsPtr, session);
  QSet<Process>::const_iterator it = session->procs()->constFind(p);
  if (it == session->procs()->constEnd())
  {
    // New process: add it to the session and return it.
    session->procs()->insert(p);
    return p;
  }
  else
  {
    // Already exists: return the existing version and let our new one destroy
    // itself by going out of scope.
    return *it;
  }
}

Process::~Process()
{
}

QList<QPair<Event, Process> > Process::transitions() const
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
      Process p = Process::create(hsProcs[i], _d->session);
      _d->next.append(QPair<Event, Process>(e, p));
    }

    free(hsProcs);
    free(hsEvents);
    _d->loaded = true;
  }

  return _d->next;
}

DisplayString Process::displayText() const
{
  if (_d->displayText == DisplayString())
  {
    wchar_t * str = NULL;
    cpex_process_string(_d->session->getHsPtr(), _d->hsPtr, true, &str);
    _d->displayText = DisplayString(QString::fromWCharArray(str));
    free(str);
  }
  return _d->displayText;
}

QString Process::fullText() const
{
  if (_d->fullText == QString())
  {
    wchar_t * str = NULL;
    cpex_process_string(_d->session->getHsPtr(), _d->hsPtr, false, &str);
    _d->fullText = QString::fromWCharArray(str).replace(QRegExp("\\s+"), " ");
    free(str);
  }
  return _d->fullText;
}

bool Process::isValid() const
{
  return _d;
}

const CSPMSession * Process::session() const
{
  return _d->session;
}

bool Process::operator ==(const Process & other) const
{
  // Equate two processes if they are the same on the backend.
  return _d->backend == other._d->backend;
}

uint Process::hash() const
{
  return cpex_process_hash(_d->hsPtr);
}

const Process & Process::operator =(const Process & other)
{
  _d = other._d;
  return *this;
}

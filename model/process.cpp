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
      _d->backend = new PSynchronisingExternalChoice(hsPtr, session);
      break;
    case 15:
      _d->backend = new PSynchronisingInterrupt(hsPtr, session);
      break;
    case 16:
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
    cpex_transitions(_d->session->getHsPtr(), _d->hsPtr, true, &hsEvents, &hsProcs,
      &transitionCount);

    for (quint32 i = 0; i < transitionCount; i++)
    {
      Process p = Process::create(hsProcs[i], _d->session);
      _d->next.append(QPair<Event, Process>(Event::create(
        _d->session, hsEvents[i]), p));
    }

    free(hsProcs);
    free(hsEvents);
    _d->loaded = true;
  }

  return _d->next;
}

QList<Process> Process::components(bool expandCall) const
{
  QList<Process> ret;
  switch (_d->backend->type)
  {
    // Unary ops
    case PBase::Hide:
    case PBase::Operator:
    case PBase::Prefix:
    case PBase::Rename:
    {
      PUnary * b = static_cast<PUnary *>(_d->backend);
      ret.append(b->opProcess());
      break;
    }
    // Binary ops
    case PBase::Exception:
    case PBase::Interrupt:
    case PBase::LinkParallel:
    case PBase::SequentialComp:
    case PBase::SlidingChoice:
    case PBase::SynchronisingInterrupt:
    {
      PBinary * b = static_cast<PBinary *>(_d->backend);
      ret.append(b->opProcess2().first);
      ret.append(b->opProcess2().second);
      break;
    }
    // N-ary ops
    case PBase::AlphaParallel:
    case PBase::ExternalChoice:
    case PBase::GenParallel:
    case PBase::Interleave:
    case PBase::InternalChoice:
    case PBase::SynchronisingExternalChoice:
    {
      PNary * b = static_cast<PNary *>(_d->backend);
      ret = b->opProcesses();
      break;
    }
    // ProcCall - show successors if requested.
    case PBase::ProcCall:
    {
      if (expandCall)
      {
        PProcCall * b = static_cast<PProcCall *>(_d->backend);
        ret.append(b->opProcCall().first);
      }
      break;
    }
  }
  return ret;
}

bool Process::offersEvent(Event event) const
{
  QPair<Event, Process> t;
  foreach (t, transitions())
  {
    if (t.first == event)
    {
      return true;
    }
  }
  return false;
}

QList<Event> Process::offeredEvents(const QList<Event> & events) const
{
  QPair<Event, Process> t;
  QList<Event> offered;
  foreach (t, transitions())
  {
    if (events.isEmpty() || events.contains(t.first))
    {
      offered.append(t.first);
    }
  }
  return offered;
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
    _d->fullText = QString::fromWCharArray(str);
    free(str);
  }
  return _d->fullText;
}

QString Process::toolTip() const
{
  return _d->backend->toolTip();
}

QString Process::whyEvent(const QList<Event> & events, bool atRoot) const
{
  if (events.isEmpty())
  {
    return atRoot ? QObject::tr("The event given is not valid.")
      : QObject::tr("This component is not relevant to your query.");
  }
  QList<Event> passEvents = offeredEvents(events);
  if (passEvents.isEmpty())
  {
    return _d->backend->whyEvent(events);
  }
  return QObject::tr("This component offers the event(s) %1.", 0, events.count())
    .arg(Event::displayEventList(_d->session, events, Event::SetOrSingle));
}

QHash<int, QList<Event> > Process::eventsRequiredBySuccessors(
  const QList<Event> & events) const
{
  if (events.isEmpty())
  {
    return QHash<int, QList<Event> >();
  }
  return _d->backend->successorEvents(events);
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
  return cpex_process_equal(_d->hsPtr, other._d->hsPtr);
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

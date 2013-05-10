#include "event.h"

#include "haskell/Cpex/Foreign_stub.h"
#include <HsFFI.h>

#include "cspmsession.h"

class EventData : public QSharedData
{
public:
  EventData(const CSPMSession * session, void * hsPtr) : hsPtr(hsPtr),
    session(session)
  {
  }

  EventData(const EventData & other) : QSharedData(other), hsPtr(other.hsPtr),
    session(other.session), displayText(other.displayText), type(other.type)
  {
  }

  ~EventData()
  {
    hs_free_stable_ptr(hsPtr);
  }

  void * hsPtr;
  const CSPMSession * session;
  mutable QString displayText;
  mutable Event::Type type;
};

Event::Event()
{
}

Event::Event(const Event & other) : _d(other._d)
{
}

Event::Event(const CSPMSession * session, void * hsPtr)
{
  _d = new EventData(session, hsPtr);
}

Event::~Event()
{
}

Event Event::create(const CSPMSession * session, void * hsPtr)
{
  Event e = session->events()->value((size_t) hsPtr);
  if (!e.isValid())
  {
    e = Event(session, hsPtr);
    session->events()->insert((size_t) hsPtr, e);
  }
  return e;
}

bool Event::isValid() const
{
  return _d;
}

void Event::_lazyLoad() const
{
  _d->type = User;
  wchar_t * name = NULL;
  cpex_event_string(_d->session->getHsPtr(), _d->hsPtr, &name, &(_d->type));
  _d->displayText = QString::fromWCharArray(name);
  free(name);
}

QString Event::displayText() const
{
  if (!isValid())
  {
    return QString();
  }

  if (_d->displayText == QString())
  {
    _lazyLoad();
  }
  return _d->displayText;
}

Event::Type Event::type() const
{
  if (!isValid())
  {
    return Event::Tau;
  }

  if (_d->displayText == QString())
  {
    _lazyLoad();
  }
  return _d->type;
}

bool Event::operator ==(const Event & other) const
{
  if (!isValid())
  {
    return !other.isValid();
  }
  return other.isValid() &&
    (_d == other._d || _d->hsPtr == other._d->hsPtr);
}

Event & Event::operator =(const Event & other)
{
  _d = other._d;
  return *this;
}

QString Event::displayEventList(const CSPMSession * session, QList<Event> events,
  ListStyle style)
{
  if (events.isEmpty())
  {
    // Empty set symbol or empty string.
    return (style == Event::Set || style == Event::SetOrSingle) ?
      QChar(0x2205) : QString();
  }
  if (style != Event::Set && events.size() == 1)
  {
    return events[0].displayText();
  }

  if (style == Event::Set || style == Event::SetOrSingle)
  {
    // evs is an array of haskell StablePtrs.
    void ** evs = (void **) malloc(sizeof(void *) * events.count());
    for (int i = 0; i < events.count(); i++)
    {
      evs[i] = events[i]._d->hsPtr;
    }
    wchar_t * str = NULL;
    cpex_events_string(session->getHsPtr(), evs, events.count(), &str);
    QString ret = QString::fromWCharArray(str);
    free(evs);
    free(str);
    return ret;
  }
  else
  {
    QStringList ret;
    QString extra;
    foreach (Event e, events)
    {
      ret << e.displayText();
    }

    if (style == Event::CommaAnd)
    {
      extra = QObject::tr(" and %1").arg(ret.takeLast());
    }
    else if (style == Event::CommaOr)
    {
      extra = QObject::tr(" or %1").arg(ret.takeLast());
    }
    return ret.join(", ") + extra;
  }
}

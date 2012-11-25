#include "event.h"

#include "haskell/Cpex/Foreign_stub.h"
#include <HsFFI.h>

class EventData : public QSharedData
{
public:
  EventData(void * hsPtr) : hsPtr(hsPtr)
  {
  }

  EventData(const EventData & other) : QSharedData(other), hsPtr(other.hsPtr),
    displayText(other.displayText), type(other.type)
  {
  }

  ~EventData()
  {
    hs_free_stable_ptr(hsPtr);
  }

  void * hsPtr;
  mutable QString displayText;
  mutable Event::Type type;
};

Event::Event()
{
}

Event::Event(const Event & other) : _d(other._d)
{
}

Event::Event(void * hsPtr)
{
  _d = new EventData(hsPtr);
}

Event::~Event()
{
}

void Event::_lazyLoad() const
{
  _d->type = User;
  wchar_t * name = NULL;
  cpex_event_string(_d->hsPtr, &name, &(_d->type));
  _d->displayText = QString::fromWCharArray(name);
  free(name);
}

QString Event::displayText() const
{
  if (!_d)
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
  if (!_d)
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
  return displayText() == other.displayText();
}

Event & Event::operator =(const Event & other)
{
  _d = other._d;
  return *this;
}

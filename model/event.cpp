#include "event.h"

#include "haskell/Cpex/Foreign_stub.h"
#include <HsFFI.h>

Event::Event(void * hsPtr) : _hsPtr(hsPtr)
{
}

Event::~Event()
{
  hs_free_stable_ptr(_hsPtr);
}

void Event::_lazyLoad() const
{
  _type = User;
  wchar_t * name = NULL;
  cpex_event_string(_hsPtr, &name, &_type);
  _displayText = QString::fromWCharArray(name);
  free(name);
}

QString Event::displayText() const
{
  if (_displayText == NULL)
  {
    _lazyLoad();
  }
  return _displayText;
}

Event::Type Event::type() const
{
  return _type;
}

bool Event::operator ==(const Event & other) const
{
  return displayText() == other.displayText();
}

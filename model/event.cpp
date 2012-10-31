#include "event.h"

#include "haskell/Cpex/Transitions_stub.h"
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
/*
  QString eventStr;
  switch (type)
  {
    case 0:
      eventStr = QString::fromWCharArray(name);
    case 1:
      eventStr = "τ";
    case 2:
      eventStr = "✓";
  } */
  _type = 0;
  wchar_t * name = NULL;
  cpex_event_string(_hsPtr, &name, &_type);
  _displayText = QString::fromWCharArray(name);
}

QString Event::displayText() const
{
  if (_displayText == NULL)
  {
    _lazyLoad();
  }
  return _displayText;
}

bool Event::operator ==(const Event & other) const
{
  return displayText() == other.displayText();
}

#include "transition.h"

Transition::Transition(const QString & event, Process * result)
{
  _event = event;
  _result = result;
}

QString Transition::event() const
{
  return _event;
}

Process * Transition::result() const
{
  return _result;
}

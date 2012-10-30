#include "process.h"

#include "haskell/Cpex/Transitions_stub.h"

Process::Process(void * hsPtr)
{
  _hsPtr = hsPtr;
}

Process::~Process()
{
  delete _loadedTransitions;
  // TODO: Investigate whether we need to do anything about _hsTransitions
}

QLinkedList<Transition> * Process::transitions()
{
  if (_loadedTransitions == NULL)
  {
    _hsTransitions = NULL;
    quint32 transitionCount = 0;
    // TODO: Error handling.
    cpex_transitions(_hsPtr, &_hsTransitions, &transitionCount);

    _loadedTransitions = new QLinkedList<Transition>();
    for (quint32 i = 0; i < transitionCount; i++)
    {
      void * event = NULL;
      wchar_t * name = NULL;
      quint32 type = 0;
      cpex_event_data(_hsTransitions[i], &event);
      cpex_event_string(event, &name, &type);

      QString eventStr;
      switch (type)
      {
        case 0:
          eventStr = QString::fromWCharArray(name);
        case 1:
          eventStr = "τ";
        case 2:
          eventStr = "✓";
      }
      *_loadedTransitions << Transition(eventStr, this);
    }
  }

  return _loadedTransitions;
}

QString Process::displayText() const
{
  // TODO
  return "[placeholder]";
}

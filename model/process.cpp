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
      wchar_t* name = NULL;
      quint32 type = 0;
      cpex_event_data(_hsTransitions[i], &name, &type);

      QString event;
      switch (type)
      {
        case 0:
          event = QString::fromWCharArray(name);
        case 1:
          event = "τ";
        case 2:
          event = "✓";
      }
      *_loadedTransitions << Transition(event, this);
    }
  }

  return _loadedTransitions;
}

QString Process::displayText() const
{
  // TODO
  return "[placeholder]";
}

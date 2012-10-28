#ifndef PROCESS_H
#define PROCESS_H

#include "transition.h"
#include <QLinkedList>

// Forward-declare Transition due to circular dependency.
class Transition;

class Process
{
public:
  Process(void * hsPtr);
  ~Process();
  QLinkedList<Transition> * transitions();
  QString displayText() const;

private:
  void * _hsPtr;
  QLinkedList<Transition> * _loadedTransitions;
  void ** _hsTransitions;
};

#endif // PROCESS_H

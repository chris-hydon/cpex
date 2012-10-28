#ifndef TRANSITION_H
#define TRANSITION_H

#include <QString>
#include "model/process.h"

// Forward-declare Process due to circular dependency.
class Process;

class Transition
{
public:
  Transition(const QString & event, Process * result);
  QString event() const;
  Process * result() const;

private:
  QString _event;
  Process * _result;
};

#endif // TRANSITION_H

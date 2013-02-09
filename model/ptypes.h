#ifndef PBASE_H
#define PBASE_H

#include "event.h"
#include "process.h"

#include <QHash>
#include <QList>

class PAlphaParallel;
class PException;
class PExternalChoice;
class PGenParallel;
class PHide;
class PInternalChoice;
class PInterleave;
class PInterrupt;
class PLinkParallel;
class POperator;
class PPrefix;
class PRename;
class PSequentialComp;
class PSlidingChoice;
class PProcCall;

class PBase
{
public:
  enum PType
  {
    AlphaParallel, Exception, ExternalChoice, GenParallel, Hide, InternalChoice,
    Interleave, Interrupt, LinkParallel, Operator, Prefix, Rename, SequentialComp,
    SlidingChoice, ProcCall
  };
  const PType type;

  // Use the visitor pattern to test equality. Default to false, then allow each
  // type to implement its own isEqual.
  virtual bool isEqual(const PAlphaParallel *) const { return false; }
  virtual bool isEqual(const PException *) const { return false; }
  virtual bool isEqual(const PExternalChoice *) const { return false; }
  virtual bool isEqual(const PGenParallel *) const { return false; }
  virtual bool isEqual(const PHide *) const { return false; }
  virtual bool isEqual(const PInternalChoice *) const { return false; }
  virtual bool isEqual(const PInterleave *) const { return false; }
  virtual bool isEqual(const PInterrupt *) const { return false; }
  virtual bool isEqual(const PLinkParallel *) const { return false; }
  virtual bool isEqual(const POperator *) const { return false; }
  virtual bool isEqual(const PPrefix *) const { return false; }
  virtual bool isEqual(const PRename *) const { return false; }
  virtual bool isEqual(const PSequentialComp *) const { return false; }
  virtual bool isEqual(const PSlidingChoice *) const { return false; }
  virtual bool isEqual(const PProcCall *) const { return false; }

  // Each implementation must call other.isEqual(this) to complete the visitor loop.
  virtual bool operator ==(const PBase & other) const = 0;

  // Operators may give a tool tip.
  virtual QString toolTip() const { return QString(); }

// Protected methods in PBase can be exposed in the subclasses that require them
// with the "using" keyword-> A subclass will use at most one of opEvent, opEvents,
// opEventMap and opAlphabets, and at most one of opProcess, opProcess2, opProcesses
// and opProcCall->
protected:
  PBase(void *, const CSPMSession *, PType);

  Event opEvent() const;
  QList<Event> opEvents() const;
  QHash<Event, Event> opEventMap() const;
  QList<QList<Event> > opAlphabets() const;

  Process opProcess() const;
  QPair<Process, Process> opProcess2() const;
  QList<Process> opProcesses() const;
  QPair<Process, QString> opProcCall() const;

  mutable QString _text;

private:
  void * _hsPtr;
  const CSPMSession * _session;
  mutable bool _loadedEvent;
  mutable bool _loadedProcess;
  mutable QHash<Event, Event> _eventMap;
  mutable QList<QList<Event> > _alphabets;
  mutable QList<Process> _processes;
};

class PUnary : public PBase
{
public:
  using PBase::opProcess;
protected:
  PUnary(void * p, const CSPMSession * s, PType t) : PBase(p, s, t) {}
};

class PBinary : public PBase
{
public:
  using PBase::opProcess2;
protected:
  PBinary(void * p, const CSPMSession * s, PType t) : PBase(p, s, t) {}
};

class PNary : public PBase
{
public:
  using PBase::opProcesses;
protected:
  PNary(void * p, const CSPMSession * s, PType t) : PBase(p, s, t) {}
};

class PAlphaParallel : public PNary
{
public:
  using PBase::opAlphabets;
  PAlphaParallel(void * p, const CSPMSession * s) : PNary(p, s, AlphaParallel) {}
  virtual QString toolTip() const;

  virtual bool operator ==(const PBase & other) const
  {
    return other.isEqual(this);
  }

  virtual bool isEqual(const PAlphaParallel * other) const
  {
    return opProcesses() == other->opProcesses() &&
      opAlphabets() == other->opAlphabets();
  }
};

class PException : public PBinary
{
public:
  using PBase::opEvents;
  PException(void * p, const CSPMSession * s) : PBinary(p, s, Exception) {}

  virtual bool operator ==(const PBase & other) const
  {
    return other.isEqual(this);
  }

  virtual bool isEqual(const PException * other) const
  {
    return opProcess2() == other->opProcess2() &&
      opEvents() == other->opEvents();
  }
};

class PExternalChoice : public PNary
{
public:
  PExternalChoice(void * p, const CSPMSession * s) : PNary(p, s, ExternalChoice) {}

  virtual bool operator ==(const PBase & other) const
  {
    return other.isEqual(this);
  }

  virtual bool isEqual(const PExternalChoice * other) const
  {
    return opProcesses() == other->opProcesses();
  }
};

class PGenParallel : public PNary
{
public:
  using PBase::opEvents;
  PGenParallel(void * p, const CSPMSession * s) : PNary(p, s, GenParallel) {}

  virtual bool operator ==(const PBase & other) const
  {
    return other.isEqual(this);
  }

  virtual bool isEqual(const PGenParallel * other) const
  {
    return opProcesses() == other->opProcesses() &&
      opEvents() == other->opEvents();
  }
};

class PHide : public PUnary
{
public:
  using PBase::opEvents;
  PHide(void * p, const CSPMSession * s) : PUnary(p, s, Hide) {}

  virtual bool operator ==(const PBase & other) const
  {
    return other.isEqual(this);
  }

  virtual bool isEqual(const PHide * other) const
  {
    return opProcesses() == other->opProcesses() &&
      opEvents() == other->opEvents();
  }
};

class PInternalChoice : public PNary
{
public:
  PInternalChoice(void * p, const CSPMSession * s) : PNary(p, s, InternalChoice) {}

  virtual bool operator ==(const PBase & other) const
  {
    return other.isEqual(this);
  }

  virtual bool isEqual(const PInternalChoice * other) const
  {
    return opProcesses() == other->opProcesses();
  }
};

class PInterleave : public PNary
{
public:
  PInterleave(void * p, const CSPMSession * s) : PNary(p, s, Interleave) {}

  virtual bool operator ==(const PBase & other) const
  {
    return other.isEqual(this);
  }

  virtual bool isEqual(const PInterleave * other) const
  {
    return opProcesses() == other->opProcesses();
  }
};

class PInterrupt : public PBinary
{
public:
  PInterrupt(void * p, const CSPMSession * s) : PBinary(p, s, Interrupt) {}

  virtual bool operator ==(const PBase & other) const
  {
    return other.isEqual(this);
  }

  virtual bool isEqual(const PInterrupt * other) const
  {
    return opProcess2() == other->opProcess2();
  }
};

class PLinkParallel : public PBinary
{
public:
  using PBase::opEventMap;
  PLinkParallel(void * p, const CSPMSession * s) : PBinary(p, s, LinkParallel) {}

  virtual bool operator ==(const PBase & other) const
  {
    return other.isEqual(this);
  }

  virtual bool isEqual(const PLinkParallel * other) const
  {
    return opProcess2() == other->opProcess2() &&
      opEventMap() == other->opEventMap();
  }
};

class POperator : public PUnary
{
public:
  POperator(void * p, const CSPMSession * s) : PUnary(p, s, Operator) {}

  virtual bool operator ==(const PBase & other) const
  {
    return other.isEqual(this);
  }

  virtual bool isEqual(const POperator * other) const
  {
    return opProcess() == other->opProcess();
  }
};

class PPrefix : public PUnary
{
public:
  using PBase::opEvent;
  PPrefix(void * p, const CSPMSession * s) : PUnary(p, s, Prefix) {}

  virtual bool operator ==(const PBase & other) const
  {
    return other.isEqual(this);
  }

  virtual bool isEqual(const PPrefix * other) const
  {
    return opProcess() == other->opProcess() &&
      opEvent() == other->opEvent();
  }
};

class PRename : public PUnary
{
public:
  using PBase::opEventMap;
  PRename(void * p, const CSPMSession * s) : PUnary(p, s, Rename) {}

  virtual bool operator ==(const PBase & other) const
  {
    return other.isEqual(this);
  }

  virtual bool isEqual(const PRename * other) const
  {
    return opProcess() == other->opProcess() &&
      opEventMap() == other->opEventMap();
  }
};

class PSequentialComp : public PBinary
{
public:
  PSequentialComp(void * p, const CSPMSession * s) : PBinary(p, s, SequentialComp) {}

  virtual bool operator ==(const PBase & other) const
  {
    return other.isEqual(this);
  }

  virtual bool isEqual(const PSequentialComp * other) const
  {
    return opProcess2() == other->opProcess2();
  }
};

class PSlidingChoice : public PBinary
{
public:
  PSlidingChoice(void * p, const CSPMSession * s) : PBinary(p, s, SlidingChoice) {}

  virtual bool operator ==(const PBase & other) const
  {
    return other.isEqual(this);
  }

  virtual bool isEqual(const PSlidingChoice * other) const
  {
    return opProcess2() == other->opProcess2();
  }
};

class PProcCall : public PBase
{
public:
  using PBase::opProcCall;
  PProcCall(void * p, const CSPMSession * s) : PBase(p, s, ProcCall) {}

  virtual bool operator ==(const PBase & other) const
  {
    return other.isEqual(this);
  }

  virtual bool isEqual(const PProcCall * other) const
  {
    return opProcCall().second == other->opProcCall().second;
  }
};

#endif // PBASE_H

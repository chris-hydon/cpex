#ifndef PBASE_H
#define PBASE_H

#include "event.h"
#include "process.h"

#include <QCoreApplication>
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
class PSynchronisingExternalChoice;
class PSynchronisingInterrupt;
class PProcCall;

class PBase
{
public:
  enum PType
  {
    AlphaParallel, Exception, ExternalChoice, GenParallel, Hide, InternalChoice,
    Interleave, Interrupt, LinkParallel, Operator, Prefix, Rename, SequentialComp,
    SlidingChoice, SynchronisingExternalChoice, SynchronisingInterrupt, ProcCall,
    Invalid
  };
  const PType type;

  virtual ~PBase() {}

  /**
   * Gives the tool tip text. Default implementation is the empty string.
   */
  virtual QString toolTip() const { return QString(); }

  /**
   * Explains why, given a list of events, no event in the list is offered. This
   * method is assumed only to be called when it is in fact true that no event in
   * the list is offered. The boolean parameter is true if asynchronous termination
   * semantics should be used, or false if not.
   */
  virtual QString whyEvent(const QList<Event> &, bool) const = 0;

  /**
   * Given a list of events, for each component (indexed by its appearance in the
   * return value of components), gives a list of all events that should be offered
   * by that component in order that its part in ensuring that any one of the input
   * events is offered is fulfilled.
   */
  virtual QHash<int, QList<Event> > successorEvents(const QList<Event> &, bool)
    const = 0;

  /**
   * Returns the processes which are components of this process. This standardises
   * the outputs of opProcess, opProcess2, opProcesses and opProcCall in PUnary,
   * PBinary, PNary and PProcCall respectively.
   */
  virtual QList<Process> components() const = 0;

protected:
  // Required for compilation. This constructor should never be invoked: virtual
  // inheritance should prevent it.
  PBase() : type(Invalid) {}
  // This constructor should be invoked explicitly by concrete classes. We don't let
  // intermediate types like PUnary call this constructor because virtual inheritance
  // will ignore the call, possibly causing hard to debug errors.
  PBase(void *, const CSPMSession *, PType);

  const CSPMSession * _session;
  void * _hsPtr;
};

/*
Process types: Unary, Binary or Nary. The appropriate class is inherited by each
concrete class representing a process of that type.
*/

class PUnary : public virtual PBase
{
public:
  Process opProcess() const;
  virtual QList<Process> components() const
  {
    QList<Process> r;
    r.append(opProcess());
    return r;
  }

protected:
  PUnary() : _loadedProcess(false) {}

private:
  mutable bool _loadedProcess;
  mutable Process _process;
};

class PBinary : public virtual PBase
{
public:
  QPair<Process, Process> opProcess2() const;
  virtual QList<Process> components() const
  {
    QList<Process> r;
    r << opProcess2().first;
    r << opProcess2().second;
    return r;
  }

protected:
  PBinary() : _loadedProcess(false) {}

private:
  mutable bool _loadedProcess;
  mutable QPair<Process, Process> _processes;
};

class PNary : public virtual PBase
{
public:
  QList<Process> opProcesses() const;
  virtual QList<Process> components() const
  {
    return opProcesses();
  }

protected:
  PNary() : _loadedProcess(false) {}

private:
  mutable bool _loadedProcess;
  mutable QList<Process> _processes;
};

/*
Parameter types: some process types take events as parameters. Currently the only
forms shared by more than one concrete type are event lists and event maps.
Alphabets (list of lists of events) and single events are held only by Prefix and
Alphabetised Parallel respectively, so are defined in the concrete class. Refactor
later if necessary.
*/

class PWithEventList : public virtual PBase
{
public:
  QList<Event> opEvents() const;
protected:
  PWithEventList() : _loadedEvent(false) {}
private:
  mutable bool _loadedEvent;
  mutable QList<Event> _events;
};

class PWithEventMap : public virtual PBase
{
public:
  QList<QPair<Event, Event> > opEventMap() const;
protected:
  PWithEventMap() : _loadedEvent(false) {}
private:
  mutable bool _loadedEvent;
  mutable QList<QPair<Event, Event> > _eventMap;
};

class PAlphaParallel : public PNary
{
  Q_DECLARE_TR_FUNCTIONS(PAlphaParallel)

public:
  QList<QList<Event> > opAlphabets() const;
  PAlphaParallel(void * p, const CSPMSession * s) : PBase(p, s, AlphaParallel),
    _loadedEvent(false) {}
  virtual QString toolTip() const;
  virtual QString whyEvent(const QList<Event> &, bool) const;
  virtual QHash<int, QList<Event> > successorEvents(const QList<Event> &, bool)
    const;

private:
  mutable bool _loadedEvent;
  mutable QList<QList<Event> > _alphabets;
};

class PException : public PBinary, public PWithEventList
{
  Q_DECLARE_TR_FUNCTIONS(PException)

public:
  PException(void * p, const CSPMSession * s) : PBase(p, s, Exception) {}
  virtual QString toolTip() const;
  virtual QString whyEvent(const QList<Event> &, bool)
   const;
  virtual QHash<int, QList<Event> > successorEvents(const QList<Event> &, bool)
    const;
};

class PExternalChoice : public PNary
{
  Q_DECLARE_TR_FUNCTIONS(PExternalChoice)

public:
  PExternalChoice(void * p, const CSPMSession * s) : PBase(p, s, ExternalChoice) {}
  virtual QString toolTip() const;
  virtual QString whyEvent(const QList<Event> &, bool) const;
  virtual QHash<int, QList<Event> > successorEvents(const QList<Event> &, bool)
    const;
};

class PGenParallel : public PNary, public PWithEventList
{
  Q_DECLARE_TR_FUNCTIONS(PGenParallel)

public:
  PGenParallel(void * p, const CSPMSession * s) : PBase(p, s, GenParallel) {}
  virtual QString toolTip() const;
  virtual QString whyEvent(const QList<Event> &, bool) const;
  virtual QHash<int, QList<Event> > successorEvents(const QList<Event> &, bool)
    const;
};

class PHide : public PUnary, public PWithEventList
{
  Q_DECLARE_TR_FUNCTIONS(PHide)

public:
  PHide(void * p, const CSPMSession * s) : PBase(p, s, Hide) {}
  virtual QString toolTip() const;
  virtual QString whyEvent(const QList<Event> &, bool) const;
  virtual QHash<int, QList<Event> > successorEvents(const QList<Event> &, bool)
    const;
};

class PInternalChoice : public PNary
{
  Q_DECLARE_TR_FUNCTIONS(PInternalChoice)

public:
  PInternalChoice(void * p, const CSPMSession * s) : PBase(p, s, InternalChoice) {}
  virtual QString toolTip() const;
  virtual QString whyEvent(const QList<Event> &, bool) const;
  virtual QHash<int, QList<Event> > successorEvents(const QList<Event> &, bool)
    const;
};

class PInterleave : public PNary
{
  Q_DECLARE_TR_FUNCTIONS(PInterleave)

public:
  PInterleave(void * p, const CSPMSession * s) : PBase(p, s, Interleave) {}
  virtual QString toolTip() const;
  virtual QString whyEvent(const QList<Event> &, bool) const;
  virtual QHash<int, QList<Event> > successorEvents(const QList<Event> &, bool)
    const;
};

class PInterrupt : public PBinary
{
  Q_DECLARE_TR_FUNCTIONS(PInterrupt)

public:
  PInterrupt(void * p, const CSPMSession * s) : PBase(p, s, Interrupt) {}
  virtual QString toolTip() const;
  virtual QString whyEvent(const QList<Event> &, bool) const;
  virtual QHash<int, QList<Event> > successorEvents(const QList<Event> &, bool)
    const;
};

class PLinkParallel : public PBinary, public PWithEventMap
{
  Q_DECLARE_TR_FUNCTIONS(PLinkParallel)

public:
  PLinkParallel(void * p, const CSPMSession * s) : PBase(p, s, LinkParallel) {}
  virtual QString toolTip() const;
  virtual QString whyEvent(const QList<Event> &, bool) const;
  virtual QHash<int, QList<Event> > successorEvents(const QList<Event> &, bool)
    const;
};

class POperator : public PUnary
{
  Q_DECLARE_TR_FUNCTIONS(POperator)

public:
  POperator(void * p, const CSPMSession * s) : PBase(p, s, Operator) {}
  virtual QString whyEvent(const QList<Event> &, bool) const;
  virtual QHash<int, QList<Event> > successorEvents(const QList<Event> &, bool)
    const;
};

class PPrefix : public PUnary
{
  Q_DECLARE_TR_FUNCTIONS(PPrefix)

public:
  Event opEvent() const;
  PPrefix(void * p, const CSPMSession * s) : PBase(p, s, Prefix),
    _loadedEvent(false) {}
  virtual QString whyEvent(const QList<Event> &, bool) const;
  virtual QHash<int, QList<Event> > successorEvents(const QList<Event> &, bool)
    const;

private:
  mutable bool _loadedEvent;
  mutable Event _event;
};

class PRename : public PUnary, public PWithEventMap
{
  Q_DECLARE_TR_FUNCTIONS(PRename)

public:
  PRename(void * p, const CSPMSession * s) : PBase(p, s, Rename) {}
  virtual QString toolTip() const;
  virtual QString whyEvent(const QList<Event> &, bool) const;
  virtual QHash<int, QList<Event> > successorEvents(const QList<Event> &, bool)
    const;
};

class PSequentialComp : public PBinary
{
  Q_DECLARE_TR_FUNCTIONS(PSequentialComp)

public:
  PSequentialComp(void * p, const CSPMSession * s) : PBase(p, s, SequentialComp) {}
  virtual QString toolTip() const;
  virtual QString whyEvent(const QList<Event> &, bool) const;
  virtual QHash<int, QList<Event> > successorEvents(const QList<Event> &, bool)
    const;
};

class PSlidingChoice : public PBinary
{
  Q_DECLARE_TR_FUNCTIONS(PSlidingChoice)

public:
  PSlidingChoice(void * p, const CSPMSession * s) : PBase(p, s, SlidingChoice) {}
  virtual QString toolTip() const;
  virtual QString whyEvent(const QList<Event> &, bool) const;
  virtual QHash<int, QList<Event> > successorEvents(const QList<Event> &, bool)
    const;
};

class PSynchronisingExternalChoice : public PNary, public PWithEventList
{
  Q_DECLARE_TR_FUNCTIONS(PSynchronisingExternalChoice)

public:
  PSynchronisingExternalChoice(void * p, const CSPMSession * s) :
    PBase(p, s, SynchronisingExternalChoice) {}
  virtual QString toolTip() const;
  virtual QString whyEvent(const QList<Event> &, bool) const;
  virtual QHash<int, QList<Event> > successorEvents(const QList<Event> &, bool)
    const;
};

class PSynchronisingInterrupt : public PBinary, public PWithEventList
{
  Q_DECLARE_TR_FUNCTIONS(PSynchronisingInterrupt)

public:
  PSynchronisingInterrupt(void * p, const CSPMSession * s) :
    PBase(p, s, SynchronisingInterrupt) {}
  virtual QString toolTip() const;
  virtual QString whyEvent(const QList<Event> &, bool) const;
  virtual QHash<int, QList<Event> > successorEvents(const QList<Event> &, bool)
    const;
};

class PProcCall : public PBase
{
  Q_DECLARE_TR_FUNCTIONS(PProcCall)

public:
  QPair<Process, QString> opProcCall() const;
  PProcCall(void * p, const CSPMSession * s) : PBase(p, s, ProcCall),
    _loadedProcess(false) {}
  virtual QString whyEvent(const QList<Event> &, bool) const;
  virtual QHash<int, QList<Event> > successorEvents(const QList<Event> &, bool)
    const;
  virtual QList<Process> components() const
  {
    QList<Process> r;
    r.append(opProcCall().first);
    return r;
  }

private:
  mutable bool _loadedProcess;
  mutable QPair<Process, QString> _proccall;
};

#endif // PBASE_H

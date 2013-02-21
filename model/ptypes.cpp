#include "ptypes.h"

#include <QStringList>
#include "haskell/Cpex/Foreign_stub.h"
#include "cspmsession.h"

PBase::PBase(void * hsPtr, const CSPMSession * session, PType type) : type(type),
  _hsPtr(hsPtr), _session(session), _loadedEvent(false), _loadedProcess(false)
{
}

Event PBase::opEvent() const
{
  if (_loadedEvent)
  {
    return _alphabets.at(0).at(0);
  }

  void * event = NULL;
  cpex_op_event(_hsPtr, &event);

  QList<Event> a;
  Event e(event);
  a.append(e);
  _alphabets.append(a);

  _loadedEvent = true;
  return e;
}

QList<Event> PBase::opEvents() const
{
  if (_loadedEvent)
  {
    return _alphabets.at(0);
  }

  void ** events = NULL;
  quint32 num = 0;
  cpex_op_events(_hsPtr, &events, &num);

  QList<Event> a;
  for (quint32 i = 0; i < num; i++)
  {
    a.append(Event(events[i]));
  }
  _alphabets.append(a);

  _loadedEvent = true;
  return a;
}

QHash<Event, Event> PBase::opEventMap() const
{
  if (_loadedEvent)
  {
    return _eventMap;
  }

  void ** eventsFrom = NULL;
  void ** eventsTo = NULL;
  quint32 num = 0;
  cpex_op_event_map(_hsPtr, &eventsFrom, &eventsTo, &num);

  for (quint32 i = 0; i < num; i++)
  {
    _eventMap.insert(Event(eventsFrom[i]), Event(eventsTo[i]));
  }

  _loadedEvent = true;
  return _eventMap;
}

QList<QList<Event> > PBase::opAlphabets() const
{
  if (_loadedEvent)
  {
    return _alphabets;
  }

  int num = opProcesses().count();
  void *** alphabets = NULL;
  quint32 * nums = NULL;
  cpex_op_alphabets(_hsPtr, &alphabets, &nums);

  for (int i = 0; i < num; i++)
  {
    QList<Event> a;
    for (quint32 j = 0; j < nums[i]; j++)
    {
      a.append(Event(alphabets[i][j]));
    }
    _alphabets.append(a);
  }

  _loadedEvent = true;
  return _alphabets;
}

Process PBase::opProcess() const
{
  if (_loadedProcess)
  {
    return _processes.at(0);
  }

  void * proc = NULL;
  cpex_op_process(_hsPtr, &proc);

  _processes.append(Process::create(proc, _session));
  _loadedProcess = true;
  return _processes.at(0);
}

QPair<Process, Process> PBase::opProcess2() const
{
  if (_loadedProcess)
  {
    return QPair<Process, Process>(_processes.at(0), _processes.at(1));
  }

  void * proc1 = NULL;
  void * proc2 = NULL;
  cpex_op_process2(_hsPtr, &proc1, &proc2);

  _processes.append(Process::create(proc1, _session));
  _processes.append(Process::create(proc2, _session));
  _loadedProcess = true;
  return QPair<Process, Process>(_processes.at(0), _processes.at(1));
}

QList<Process> PBase::opProcesses() const
{
  if (_loadedProcess)
  {
    return _processes;
  }

  void ** choices = NULL;
  quint32 num = 0;
  cpex_op_processes(_hsPtr, &choices, &num);

  for (quint32 i = 0; i < num; i++)
  {
    _processes.append(Process::create(choices[i], _session));
  }
  _loadedProcess = true;
  return _processes;
}

QPair<Process, QString> PBase::opProcCall() const
{
  if (_loadedProcess)
  {
    return QPair<Process, QString>(_processes.at(0), _text);
  }

  void * proc = NULL;
  wchar_t * str = NULL;
  cpex_op_proccall(_hsPtr, &proc, &str);

  _processes.append(Process::create(proc, _session));
  _text = QString::fromWCharArray(str);
  free(str);
  _loadedProcess = true;
  return QPair<Process, QString>(_processes.at(0), _text);
}

QString PBase::displayEventList(QList<Event> events) const
{
  void ** evs = Event::hsList(events);
  wchar_t * str = NULL;
  cpex_events_string(_session->getHsPtr(), evs, events.count(), &str);
  QString ret = QString::fromWCharArray(str);
  free(evs);
  free(str);
  return ret;
}

QString PAlphaParallel::toolTip() const
{
  QString tt("<p>Alphabetized Parallel</p>");
  tt += "<table border=\"1\" cellspacing=\"0\" cellpadding=\"2\">";
  tt += "<tr><td>Component</td><td>Events</td></tr>";
  QList<Process> procs = opProcesses();
  QList<QList<Event> > alphas = opAlphabets();
  for (int i = 0; i < procs.count(); i++)
  {
    tt += QString(
      "<tr><td valign=\"middle\">%1</td><td valign=\"middle\">%2</td></tr>")
      .arg(procs[i].displayText().toString(), displayEventList(alphas[i]));
  }
  tt += "</table>";
  return tt;
}

QString PException::toolTip() const
{
  return QString("<p>Exception</p><p>Throw on events: %1</p>")
    .arg(displayEventList(opEvents()));
}

QString PExternalChoice::toolTip() const
{
  return "<p>External Choice</p>";
}

QString PGenParallel::toolTip() const
{
  return QString("<p>Generalized Parallel</p><p>Synchronize on events %1</p>")
    .arg(displayEventList(opEvents()));
}

QString PHide::toolTip() const
{
  return QString("<p>Hide</p><p>%1</p>").arg(displayEventList(opEvents()));
}

QString PInternalChoice::toolTip() const
{
  return "<p>Internal Choice</p>";
}

QString PInterleave::toolTip() const
{
  return "<p>Interleave</p>";
}

QString PInterrupt::toolTip() const
{
  return "<p>Interrupt</p>";
}

QString PLinkParallel::toolTip() const
{
  QString tt("<p>Link Parallel</p><p>{%1}</p>");
  QStringList maps;
  QHash<Event, Event> evMap = opEventMap();
  QHash<Event, Event>::const_iterator it = opEventMap().constBegin();
  for (it = evMap.constBegin(); it != evMap.constEnd(); it++)
  {
    maps << QString("%1 &harr; %2")
      .arg(it.key().displayText(), it.value().displayText());
  }
  return tt.arg(maps.join(", "));
}

QString PRename::toolTip() const
{
  QString tt("<p>Rename</p><p>{%1}</p>");
  QStringList maps;
  QHash<Event, Event> evMap = opEventMap();
  QHash<Event, Event>::const_iterator it = opEventMap().constBegin();
  for (it = evMap.constBegin(); it != evMap.constEnd(); it++)
  {
    maps << QString("%1 &larr; %2")
      .arg(it.key().displayText(), it.value().displayText());
  }
  return tt.arg(maps.join(", "));
}

QString PSequentialComp::toolTip() const
{
  return "<p>Sequential Composition</p>";
}

QString PSlidingChoice::toolTip() const
{
  return "<p>Sliding Choice</p>";
}

QString PAlphaParallel::whyEvent(const Event & event) const
{
  switch (event.type())
  {
    case Event::Tau:
      return QObject::tr("none of the components offer a %1.").arg(QChar(0x03c4));
    case Event::Tick:
      return QObject::tr("not all of the components offer a %1.").arg(QChar(0x2713));
    default:
      ;
  }

  QList<Process> components = opProcesses();
  QList<QList<Event> > alphabets = opAlphabets();
  QStringList needed;
  for (int i = 0; i < components.count(); i++)
  {
    if (alphabets[i].contains(event))
    {
      needed << QString::number(i + 1);
    }
  }

  if (needed.count() == 0)
  {
    return QObject::tr("the event is not in the alphabet for any component.");
  }
  return QObject::tr("the event is not offered by the required component(s): %1", "",
    needed.count()).arg(needed.join(", "));
}

QString PException::whyEvent(const Event &) const
{
  return QObject::tr("the main process (component 1) does not offer the event.");
}

QString PExternalChoice::whyEvent(const Event &) const
{
  return QObject::tr("none of the components offer the event.");
}

QString PGenParallel::whyEvent(const Event & event) const
{
  if (opEvents().contains(event))
  {
    return QObject::tr("the event is in the interface, and not all of the "
      "components offer it.");
  }
  return QObject::tr("none of the components offer the event.");
}

QString PHide::whyEvent(const Event & event) const
{
  if (event.type() == Event::Tau)
  {
    return QObject::tr("of the events to be hidden, none of them are offered.");
  }
  else if (opEvents().contains(event))
  {
    return QObject::tr("the event is hidden here.");
  }
  return QObject::tr("the component process does not offer the event.");
}

QString PInternalChoice::whyEvent(const Event & event) const
{
  if (event.type() == Event::Tau)
  {
    return QObject::tr("there are no components.");
  }
  return QObject::tr("internal choice only offers %1 events.").arg(QChar(0x03c4));
}

QString PInterleave::whyEvent(const Event & event) const
{
  if (event.type() == Event::Tick)
  {
    return QObject::tr("not all components offer a %1.").arg(QChar(0x2713));
  }
  return QObject::tr("none of the components offer the event.");
}

QString PInterrupt::whyEvent(const Event &) const
{
  return QObject::tr("neither of the components offers the event.");
}

QString PLinkParallel::whyEvent(const Event & event) const
{
  if (event.type() == Event::Tau)
  {
    return QObject::tr("neither of the components offer a %1, and none of the "
      "synchronized event pairs are offered.").arg(QChar(0x03c4));
  }
  else if (event.type() == Event::Tick)
  {
    return QObject::tr("%1 is not offered by both components.").arg(QChar(0x2713));
  }
  else if (opProcess2().first.offersEvent(event) ||
    opProcess2().second.offersEvent(event))
  {
    return QObject::tr("the event is hidden by the link parallel operation.");
  }
  return QObject::tr("neither of the components offers the event.");
}

QString POperator::whyEvent(const Event & event) const
{
  if (event.type() == Event::Tau && opProcess().offersEvent(event))
  {
    return QObject::tr("all %1 events have been removed by the chase operator.")
      .arg(QChar(0x03c4));
  }
  return QObject::tr("the component does not offer the event.");
}

QString PPrefix::whyEvent(const Event &) const
{
  return QObject::tr("the event offered is %1.").arg(opEvent().displayText());
}

QString PProcCall::whyEvent(const Event &) const
{
  return QObject::tr("the component does not offer the event.");
}

QString PRename::whyEvent(const Event & event) const
{
  if (opEventMap().contains(event))
  {
    return QObject::tr("%1 has been renamed to %2.").arg(event.displayText(),
      opEventMap().value(event).displayText());
  }
  else
  {
    QList<Event> required = opEventMap().keys(event);
    if (!required.isEmpty())
    {
      return QObject::tr("the component does not offer any of the events renamed to "
        "this event: %1.").arg(displayEventList(required));
    }
  }
  return QObject::tr("the component does not offer the event.");
}

QString PSequentialComp::whyEvent(const Event & event) const
{
  switch (event.type())
  {
    case Event::Tau:
      return QObject::tr("the component offers neither a %1 nor a %2.")
        .arg(QChar(0x03c4), QChar(0x2713));
    case Event::Tick:
      return QObject::tr("the %1 event is hidden by the sequential composition "
        "operation.").arg(QChar(0x2713));
    default:
      return QObject::tr("the first component does not offer the event.");
  }
}

QString PSlidingChoice::whyEvent(const Event & event) const
{
  if (event.type() == Event::Tau)
  {
    return QObject::tr("there is a bug in the program. Sliding choice should "
      "ALWAYS offer %1.").arg(QChar(0x03c4));
  }
  return QObject::tr("the first component does not offer the event.");
}

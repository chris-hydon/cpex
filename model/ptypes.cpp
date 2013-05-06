#include "ptypes.h"

#include <QStringList>
#include "haskell/Cpex/Foreign_stub.h"
#include "cspmsession.h"

PBase::PBase(void * hsPtr, const CSPMSession * session, PType type) : type(type),
  _session(session), _hsPtr(hsPtr)
{
}

Event PPrefix::opEvent() const
{
  if (!_loadedEvent)
  {
    void * event = NULL;
    cpex_op_event(_session->getHsPtr(), _hsPtr, &event);

    _event = Event::create(_session, event);
    _loadedEvent = true;
  }

  return _event;
}

QList<Event> PWithEventList::opEvents() const
{
  if (!_loadedEvent)
  {
    void ** events = NULL;
    quint32 num = 0;
    cpex_op_events(_session->getHsPtr(), _hsPtr, &events, &num);

    for (quint32 i = 0; i < num; i++)
    {
      _events.append(Event::create(_session, events[i]));
    }

    free(events);
    _loadedEvent = true;
  }

  return _events;
}

QList<QPair<Event, Event> > PWithEventMap::opEventMap() const
{
  if (!_loadedEvent)
  {
    void ** eventsFrom = NULL;
    void ** eventsTo = NULL;
    quint32 num = 0;
    cpex_op_event_map(_session->getHsPtr(), _hsPtr, &eventsFrom, &eventsTo, &num);

    for (quint32 i = 0; i < num; i++)
    {
      Event from = Event::create(_session, eventsFrom[i]);
      Event to = Event::create(_session, eventsTo[i]);
      QPair<Event, Event> p(from, to);
      _eventMap.append(p);
    }

    free(eventsFrom);
    free(eventsTo);
    _loadedEvent = true;
  }

  return _eventMap;
}

QList<QList<Event> > PAlphaParallel::opAlphabets() const
{
  if (!_loadedEvent)
  {
    int num = opProcesses().count();
    void *** alphabets = NULL;
    quint32 * nums = NULL;
    cpex_op_alphabets(_session->getHsPtr(), _hsPtr, &alphabets, &nums);

    for (int i = 0; i < num; i++)
    {
      QList<Event> a;
      for (quint32 j = 0; j < nums[i]; j++)
      {
        a.append(Event::create(_session, alphabets[i][j]));
      }
      _alphabets.append(a);
      free(alphabets[i]);
    }

    free(alphabets);
    _loadedEvent = true;
  }

  return _alphabets;
}

Process PUnary::opProcess() const
{
  if (!_loadedProcess)
  {
    void * proc = NULL;
    cpex_op_process(_session->getHsPtr(), _hsPtr, &proc);

    _process = Process::create(proc, _session);
    _loadedProcess = true;
  }

  return _process;
}

QPair<Process, Process> PBinary::opProcess2() const
{
  if (!_loadedProcess)
  {
    void * proc1 = NULL;
    void * proc2 = NULL;
    cpex_op_process2(_session->getHsPtr(), _hsPtr, &proc1, &proc2);

    _processes = QPair<Process, Process>(
      Process::create(proc1, _session), Process::create(proc2, _session));
    _loadedProcess = true;
  }

  return _processes;
}

QList<Process> PNary::opProcesses() const
{
  if (!_loadedProcess)
  {
    void ** choices = NULL;
    quint32 num = 0;
    cpex_op_processes(_session->getHsPtr(), _hsPtr, &choices, &num);

    for (quint32 i = 0; i < num; i++)
    {
      _processes.append(Process::create(choices[i], _session));
    }
    free(choices);
    _loadedProcess = true;
  }

  return _processes;
}

QPair<Process, QString> PProcCall::opProcCall() const
{
  if (!_loadedProcess)
  {
    void * proc = NULL;
    wchar_t * str = NULL;
    cpex_op_proccall(_session->getHsPtr(), _hsPtr, &proc, &str);

    _proccall = QPair<Process, QString>(
      Process::create(proc, _session), QString::fromWCharArray(str));
    free(str);
    _loadedProcess = true;
  }

  return _proccall;
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
      .arg(procs[i].displayText().toString(), Event::displayEventList(_session,
        alphas[i]));
  }
  tt += "</table>";
  return tt;
}

QString PException::toolTip() const
{
  return QString("<p>Exception</p><p>Throw on events: %1</p>")
    .arg(Event::displayEventList(_session, opEvents()));
}

QString PExternalChoice::toolTip() const
{
  return "<p>External Choice</p>";
}

QString PGenParallel::toolTip() const
{
  return QString("<p>Generalized Parallel</p><p>Synchronize on events %1</p>")
    .arg(Event::displayEventList(_session, opEvents()));
}

QString PHide::toolTip() const
{
  return QString("<p>Hide</p><p>%1</p>")
    .arg(Event::displayEventList(_session, opEvents()));
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
  QList<QPair<Event, Event> > evMap = opEventMap();
  QPair<Event, Event> pair;
  foreach (pair, evMap)
  {
    maps << QString("%1 &harr; %2")
      .arg(pair.first.displayText(), pair.second.displayText());
  }
  return tt.arg(maps.join(", "));
}

QString PRename::toolTip() const
{
  QString tt("<p>Rename</p><p>{%1}</p>");
  QStringList maps;
  QList<QPair<Event, Event> > evMap = opEventMap();
  QPair<Event, Event> pair;
  foreach (pair, evMap)
  {
    maps << QString("%1 &larr; %2")
      .arg(pair.first.displayText(), pair.second.displayText());
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

QString PSynchronisingExternalChoice::toolTip() const
{
  return QString("<p>Synchronising External Choice</p>"
    "<p>Synchronize on events %1</p>")
    .arg(Event::displayEventList(_session, opEvents()));
}

QString PSynchronisingInterrupt::toolTip() const
{
  return QString("<p>Synchronising Interrupt</p><p>Synchronize on events %1</p>")
    .arg(Event::displayEventList(_session, opEvents()));
}

QHash<int, QList<Event> > _duplicateList(const QList<Event> & events, int count)
{
  QHash<int, QList<Event> > ret;
  for (int i = 0; i < count; i++)
  {
    ret.insert(i, events);
  }
  return ret;
}

QString PAlphaParallel::whyEvent(const QList<Event> & events,
  bool asyncSemantics) const
{
  QStringList complaints;
  QList<Process> components = opProcesses();
  QList<QList<Event> > alphabets = opAlphabets();
  foreach (Event event, events)
  {
    switch (event.type())
    {
      case Event::Tau:
        if (asyncSemantics)
        {
          complaints << tr("None of the components offer %1 or %2.")
            .arg(QChar(0x03c4), QChar(0x2713));
        }
        else
        {
          complaints << tr("None of the components offer %1.").arg(QChar(0x03c4));
        }
        break;
      case Event::Tick:
        if (asyncSemantics)
        {
          complaints << tr("Not all of the components are %1 (already terminated).")
            .arg(QChar(0x03a9));
        }
        else
        {
          complaints << tr("Not all of the components offer %1.").arg(QChar(0x2713));
        }
        break;
      default:
        ;
    }

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
      complaints << tr("Event %1 is not in the alphabet for any component.")
        .arg(event.displayText());
    }
    else
    {
      complaints << tr("Event %1 is not offered by the required component(s): %2.", "",
        needed.count()).arg(event.displayText(), needed.join(", "));
    }
  }
  return complaints.join("\n");
}

QHash<int, QList<Event> > PAlphaParallel::successorEvents(
  const QList<Event> & events, bool asyncSemantics) const
{
  QHash<int, QList<Event> > ret;
  QList<QList<Event> > alphabets = opAlphabets();
  for (int i = 0; i < alphabets.count(); i++)
  {
    // Want the events for component i to be the intersection of events and the
    // alphabet of component i.
    QList<Event> r;
    foreach (Event e, events)
    {
      if (e.type() == Event::Tau || (asyncSemantics && e.type() == Event::Tick) ||
        alphabets[i].contains(e))
      {
        r.append(e);
      }
    }
    ret.insert(i, r);
  }
  return ret;
}

QString PException::whyEvent(const QList<Event> & events, bool) const
{
  return tr("The main process (component 1) does not offer %1.")
    .arg(Event::displayEventList(_session, events, Event::CommaOr));
}

QHash<int, QList<Event> >
  PException::successorEvents(const QList<Event> & events, bool) const
{
  return _duplicateList(events, 1);
}

QString PExternalChoice::whyEvent(const QList<Event> & events, bool) const
{
  return tr("None of the components offer %1.", "", events.count())
    .arg(Event::displayEventList(_session, events, Event::CommaOr));
}

QHash<int, QList<Event> >
  PExternalChoice::successorEvents(const QList<Event> & events, bool) const
{
  return _duplicateList(events, opProcesses().count());
}

QString PGenParallel::whyEvent(const QList<Event> & events, bool asyncSemantics)
  const
{
  QStringList complaints;
  QList<Event> inInterface;
  QList<Event> notInInterface;
  foreach (Event event, events)
  {
    if ((!asyncSemantics && event.type() == Event::Tick) ||
      opEvents().contains(event))
    {
      inInterface << event;
    }
    else if (asyncSemantics && event.type() == Event::Tick)
    {
      complaints << tr("Not all of the components are %1 (already terminated).")
        .arg(QChar(0x03a9));
    }
    else
    {
      notInInterface << event;
    }
  }
  if (!inInterface.isEmpty())
  {
    complaints << tr("%1 is/are in the interface, and not all of the components "
      "offer it/them.", "", inInterface.count())
      .arg(Event::displayEventList(_session, inInterface, Event::CommaAnd));
  }
  if (!notInInterface.isEmpty())
  {
    complaints << tr("None of the components offer %1.", "", notInInterface.count())
      .arg(Event::displayEventList(_session, notInInterface, Event::CommaOr));
  }
  return complaints.join("\n");
}

QHash<int, QList<Event> > PGenParallel::successorEvents(
  const QList<Event> & events, bool asyncSemantics) const
{
  // For the asynchronous case, if we want a tick and can't get one, that can't be
  // fixed by offering some event. If we want a tau and can't get one, we need either
  // a tau or a tick.
  if (asyncSemantics)
  {
    QList<Event> ret;
    foreach (Event e, events)
    {
      if (e.type() == Event::Tau)
      {
        ret << e;
        ret << _session->stringToEvent("_tick");
      }
      else if (e.type() == Event::User)
      {
        ret << e;
      }
    }
    return _duplicateList(ret, opProcesses().count());
  }
  else
  {
    return _duplicateList(events, opProcesses().count());
  }
}

QString PHide::whyEvent(const QList<Event> & events, bool) const
{
  QStringList complaints;
  QList<Event> hidden;
  QList<Event> notOffered;
  foreach (Event event, events)
  {
    if (event.type() == Event::Tau)
    {
      complaints << tr("Of the events to be hidden, none of them are offered, and "
        "%1 is not offered.", "", opEvents().count()).arg(QChar(0x03c4));
    }
    else if (opEvents().contains(event))
    {
      hidden.append(event);
    }
    else
    {
      notOffered.append(event);
    }
  }
  if (!hidden.isEmpty())
  {
    complaints << tr("The event(s) %1 is/are hidden.", "", hidden.count())
      .arg(Event::displayEventList(_session, hidden, Event::CommaAnd));
  }
  if (!notOffered.isEmpty())
  {
    complaints << tr("The component process does not offer %1.", "",
      notOffered.count())
      .arg(Event::displayEventList(_session, notOffered, Event::CommaOr));
  }
  return complaints.join("\n");
}

QHash<int, QList<Event> >
  PHide::successorEvents(const QList<Event> & events, bool) const
{
  QList<Event> ret;
  // Want the events for the component to be the input, minus any events hidden,
  // plus all events hidden if a tau is in the input.
  //
  // e.g. {x,y}\{y,z} -> {x}
  //      {x,y,_tau}\{y,z} -> {x,y,z,_tau}
  foreach (Event event, events)
  {
    if (event.type() == Event::Tau)
    {
      ret.append(event);
      ret.append(opEvents());
    }
    else if (event.type() == Event::Tick || !opEvents().contains(event))
    {
      ret.append(event);
    }
  }
  return _duplicateList(ret, 1);
}

QString PInternalChoice::whyEvent(const QList<Event> & events, bool) const
{
  foreach (Event event, events) if (event.type() == Event::Tau)
  {
    return tr("There are no components.");
  }
  return tr("Internal choice only offers %1 events.").arg(QChar(0x03c4));
}

QHash<int, QList<Event> >
  PInternalChoice::successorEvents(const QList<Event> &, bool) const
{
  // Internal Choice only offers tau, no matter what its components may do.
  return QHash<int, QList<Event> >();
}

QString PInterleave::whyEvent(const QList<Event> & events, bool asyncSemantics) const
{
  QStringList complaints;
  QList<Event> nonTickEvents;
  foreach (Event event, events)
  {
    if (event.type() == Event::Tick)
    {
      if (asyncSemantics)
      {
        complaints << tr("Not all of the components are %1 (already terminated).")
          .arg(QChar(0x03a9));
      }
      else
      {
        complaints << tr("Not all components offer a %1.").arg(QChar(0x2713));
      }
    }
    else if (asyncSemantics && event.type() == Event::Tau)
    {
      complaints << tr("None of the components offer %1 or %2.")
        .arg(QChar(0x03c4), QChar(0x2713));
    }
    else
    {
      nonTickEvents.append(event);
    }
  }
  if (!nonTickEvents.isEmpty())
  {
    complaints << tr("None of the components offer %1.", "", nonTickEvents.count())
      .arg(Event::displayEventList(_session, nonTickEvents, Event::CommaOr));
  }
  return complaints.join("\n");
}

QHash<int, QList<Event> > PInterleave::successorEvents(
  const QList<Event> & events, bool asyncSemantics) const
{
  // For the asynchronous case, if we want a tick and can't get one, that can't be
  // fixed by offering some event. If we want a tau and can't get one, we need either
  // a tau or a tick.
  if (asyncSemantics)
  {
    QList<Event> ret;
    foreach (Event e, events)
    {
      if (e.type() == Event::Tau)
      {
        ret << e;
        ret << _session->stringToEvent("_tick");
      }
      else if (e.type() == Event::User)
      {
        ret << e;
      }
    }
    return _duplicateList(ret, opProcesses().count());
  }
  else
  {
    return _duplicateList(events, opProcesses().count());
  }
}

QString PInterrupt::whyEvent(const QList<Event> & events, bool) const
{
  return tr("Neither of the components offer %1.", "", events.count())
    .arg(Event::displayEventList(_session, events, Event::CommaOr));
}

QHash<int, QList<Event> >
  PInterrupt::successorEvents(const QList<Event> & events, bool) const
{
  return _duplicateList(events, 2);
}

QString PLinkParallel::whyEvent(const QList<Event> & events, bool asyncSemantics)
  const
{
  QStringList complaints;
  QList<Event> hidden;
  QList<Event> notOffered;
  foreach (Event event, events)
  {
    if (event.type() == Event::Tau)
    {
      if (asyncSemantics)
      {
        complaints <<  tr("Neither of the components offer a %1 or a %2, and none "
          "of the synchronized event pairs are offered.")
          .arg(QChar(0x03c4), QChar(0x2713));
      }
      else
      {
        complaints <<  tr("Neither of the components offer a %1, and none of the "
          "synchronized event pairs are offered.").arg(QChar(0x03c4));
      }
    }
    else if (event.type() == Event::Tick)
    {
      if (asyncSemantics)
      {
        complaints << tr("Not all of the components are %1 (already terminated).")
          .arg(QChar(0x03a9));
      }
      else
      {
        complaints << tr("%1 is not offered by both components.").arg(QChar(0x2713));
      }
    }
    else if (opProcess2().first.offersEvent(asyncSemantics, event) ||
      opProcess2().second.offersEvent(asyncSemantics, event))
    {
      hidden.append(event);
    }
    else
    {
      notOffered.append(event);
    }
  }
  if (!hidden.isEmpty())
  {
    complaints << tr("The event(s) %1 is/are hidden by the link parallel "
      "operation.", "", hidden.count())
      .arg(Event::displayEventList(_session, hidden, Event::CommaAnd));
  }
  if (!notOffered.isEmpty())
  {
    complaints << tr("Neither of the components offer %1.", "", notOffered.count())
      .arg(Event::displayEventList(_session, notOffered, Event::CommaOr));
  }
  return complaints.join("\n");
}

QHash<int, QList<Event> > PLinkParallel::successorEvents(
  const QList<Event> & events, bool asyncSemantics) const
{
  QHash<int, QList<Event> > ret;
  QList<Event> left, right;
  QSet<Event> leftHides, rightHides;
  QPair<Event, Event> pair;

  // The events required of each side are those input, minus those hidden by the
  // link parallel operation, plus all events on the appropriate side of the link
  // parallel operator if a Tau is encountered.
  foreach (pair, opEventMap())
  {
    leftHides.insert(pair.first);
    rightHides.insert(pair.second);
  }
  foreach (Event event, events)
  {
    if (event.type() == Event::Tau)
    {
      left.append(event);
      right.append(event);
      if (asyncSemantics)
      {
        Event tick = _session->stringToEvent("_tick");
        left.append(tick);
        right.append(tick);
      }
      left.append(leftHides.toList());
      right.append(rightHides.toList());
    }
    // Add non-tau events wherever they are not hidden. In the asynchronous case,
    // ticks are hidden, while in the synchronous case they are not.
    else if (!asyncSemantics || event.type() == Event::User)
    {
      if (!leftHides.contains(event)) left.append(event);
      if (!rightHides.contains(event)) right.append(event);
    }
  }
  ret.insert(0, left);
  ret.insert(1, right);
  return ret;
}

QString POperator::whyEvent(const QList<Event> & events, bool asyncSemantics) const
{
  QStringList complaints;
  bool tauChase = false;
  foreach (Event event, events)
    if (event.type() == Event::Tau && opProcess().offersEvent(asyncSemantics, event))
      tauChase = true;
  if (tauChase)
  {
    complaints << tr("All %1 events have been removed by the chase operator.")
      .arg(QChar(0x03c4));
  }
  if (events.count() > tauChase ? 1 : 0)
  {
    complaints << tr("The component does not offer %1.", "", events.count())
      .arg(Event::displayEventList(_session, events, Event::CommaOr));
  }
  return complaints.join("\n");
}

QHash<int, QList<Event> >
  POperator::successorEvents(const QList<Event> & events, bool) const
{
  return _duplicateList(events, 1);
}

QString PPrefix::whyEvent(const QList<Event> &, bool) const
{
  return tr("The event offered is %1.").arg(opEvent().displayText());
}

QHash<int, QList<Event> >
  PPrefix::successorEvents(const QList<Event> &, bool) const
{
  // What a prefix offers does not depend on its component.
  return QHash<int, QList<Event> >();
}

QString PProcCall::whyEvent(const QList<Event> & events, bool) const
{
  return tr("The component does not offer %1.", "", events.count())
    .arg(Event::displayEventList(_session, events, Event::CommaOr));
}

QHash<int, QList<Event> >
  PProcCall::successorEvents(const QList<Event> & events, bool) const
{
  return _duplicateList(events, 1);
}

QString PRename::whyEvent(const QList<Event> & events, bool) const
{
  QStringList complaints;
  QList<QPair<Event, Event> > evMap = opEventMap();
  QPair<Event, Event> pair;
  QHash<Event, QList<Event> *> mapTo;
  QHash<Event, QList<Event> *> mapFrom;
  QList<Event> notOffered;

  // Set up lists of things mapped to and from each event of interest to give better
  // performance with large amounts of data (O(n + m) instead of O(nm)).
  foreach (Event event, events)
  {
    mapTo.insert(event, new QList<Event>());
    mapFrom.insert(event, new QList<Event>());
  }
  foreach (pair, evMap)
  {
    if (pair.first == pair.second)
    {
      continue;
    }
    if (mapTo.contains(pair.first))
    {
      mapTo.value(pair.first)->append(pair.second);
    }
    if (mapFrom.contains(pair.second))
    {
      mapFrom.value(pair.second)->append(pair.first);
    }
  }

  // In renaming, an event is excluded if nothing renaming to it is offered, AND
  // one of the following is true:
  // - The event is renamed to one or more other events, and of those events none
  //   are offered.
  // - The event is not renamed, and is not offered by the component.
  foreach (Event event, events)
  {
    QList<Event> * to = mapTo.value(event);
    QList<Event> * from = mapFrom.value(event);
    if (!from->isEmpty())
    {
      complaints << tr("The component does not offer any of the events renamed to "
        "%1: %2.", "", from->count()).arg(event.displayText(),
        Event::displayEventList(_session, *from, Event::SetOrSingle));
    }
    if (!to->isEmpty())
    {
      complaints << tr("%1 has been renamed to %2.").arg(event.displayText(),
        Event::displayEventList(_session, *to, Event::SetOrSingle));
    }
    else
    {
      notOffered.append(event);
    }

    // Delete here to avoid another loop. We don't need these keys again.
    delete to;
    delete from;
  }
  if (!notOffered.isEmpty())
  {
    complaints << tr("The component does not offer %1.", "", notOffered.count())
      .arg(Event::displayEventList(_session, notOffered, Event::CommaOr));
  }
  return complaints.join("\n");
}

QHash<int, QList<Event> >
  PRename::successorEvents(const QList<Event> & events, bool) const
{
  QSet<Event> ret;
  QPair<Event, Event> pair;
  // The successor events are those input, with the inverse renaming function
  // applied, minus those renamed to something else.
  foreach (Event event, events)
  {
    bool renamed = false;
    foreach (pair, opEventMap())
    {
      if (pair.second == event)
      {
        ret.insert(pair.first);
        renamed = true;
      }
      if (pair.first == event)
      {
        renamed = true;
      }
    }
    if (!renamed)
    {
      ret.insert(event);
    }
  }
  return _duplicateList(ret.toList(), 1);
}

QString PSequentialComp::whyEvent(const QList<Event> & events, bool) const
{
  QStringList complaints;
  QList<Event> userEvents;
  foreach (Event event, events)
  {
    switch (event.type())
    {
      case Event::Tau:
        complaints << tr("The component offers neither a %1 nor a %2.")
          .arg(QChar(0x03c4), QChar(0x2713));
        break;
      case Event::Tick:
        complaints << tr("The %1 event is hidden by the sequential composition "
          "operation.").arg(QChar(0x2713));
        break;
      default:
        userEvents.append(event);
        break;
    }
  }
  if (!userEvents.isEmpty())
  {
    complaints << tr("The first component does not offer %1.", "",
      userEvents.count())
      .arg(Event::displayEventList(_session, userEvents, Event::CommaOr));
  }
  return complaints.join("\n");
}

QHash<int, QList<Event> >
  PSequentialComp::successorEvents(const QList<Event> & events, bool) const
{
  // Want the first successor to offer everything that we are looking for, plus tick
  // if a tau is requested. Do the search for tau and tick in a loop since contains()
  // is a linear-time operation anyway.
  bool tick = false, tau = false;
  foreach (Event event, events)
  {
    if (event.type() == Event::Tau) tau = true;
    if (event.type() == Event::Tick) tick = true;
  }

  // Decide whether to add a tick.
  QList<Event> ret = events;
  if (tau && !tick)
  {
    ret.append(_session->stringToEvent("_tick"));
  }
  return _duplicateList(ret, 1);
}

QString PSlidingChoice::whyEvent(const QList<Event> & events, bool) const
{
  return tr("The first component does not offer %1.", "", events.count())
    .arg(Event::displayEventList(_session, events, Event::CommaOr));
}

QHash<int, QList<Event> >
  PSlidingChoice::successorEvents(const QList<Event> & events, bool) const
{
  return _duplicateList(events, 1);
}

QString PSynchronisingExternalChoice::whyEvent(const QList<Event> & events, bool)
  const
{
  QStringList complaints;
  QList<Event> inInterface;
  QList<Event> notInInterface;
  foreach (Event event, events)
  {
    if (opEvents().contains(event))
    {
      inInterface << event;
    }
    else
    {
      notInInterface << event;
    }
  }
  if (!inInterface.isEmpty())
  {
    complaints << tr("%1 is/are in the interface, and not all of the components "
      "offer it/them.", "", inInterface.count())
      .arg(Event::displayEventList(_session, inInterface, Event::CommaAnd));
  }
  if (!notInInterface.isEmpty())
  {
    complaints << tr("None of the components offer %1.", "", notInInterface.count())
      .arg(Event::displayEventList(_session, notInInterface, Event::CommaOr));
  }
  return complaints.join("\n");
}

QHash<int, QList<Event> > PSynchronisingExternalChoice::successorEvents(
  const QList<Event> & events, bool) const
{
  return _duplicateList(events, opProcesses().count());
}

QString PSynchronisingInterrupt::whyEvent(const QList<Event> & events, bool) const
{
  QStringList complaints;
  QList<Event> inInterface;
  QList<Event> notInInterface;
  foreach (Event event, events)
  {
    if (opEvents().contains(event))
    {
      inInterface << event;
    }
    else
    {
      notInInterface << event;
    }
  }
  if (!inInterface.isEmpty())
  {
    complaints << tr("%1 is/are in the interface, and it is not the case that both "
      "of the components offer it/them.", "", inInterface.count())
      .arg(Event::displayEventList(_session, inInterface, Event::CommaAnd));
  }
  if (!notInInterface.isEmpty())
  {
    complaints << tr("Neither of the components offer %1.", "",
      notInInterface.count())
      .arg(Event::displayEventList(_session, notInInterface, Event::CommaOr));
  }
  return complaints.join("\n");
}

QHash<int, QList<Event> >
  PSynchronisingInterrupt::successorEvents(const QList<Event> & events, bool) const
{
  return _duplicateList(events, 2);
}

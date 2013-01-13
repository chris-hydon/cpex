#include "ptypes.h"

#include "haskell/Cpex/Foreign_stub.h"

PBase::PBase(void * hsPtr, const CSPMSession * session, PType type) : type(type),
  _hsPtr(hsPtr), _session(session)
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

QString PBase::opProcCall() const
{
  if (_loadedProcess)
  {
    return _text;
  }

  void * proc = NULL;
  wchar_t * str = NULL;
  cpex_op_proccall(_hsPtr, &proc, &str);

  _text = QString::fromWCharArray(str);
  free(proc);
  free(str);
  _loadedProcess = true;
  return _text;
}

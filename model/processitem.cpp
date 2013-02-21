#include "processitem.h"

ProcessItem::ProcessItem(const Process & process, const ProcessItem * parent,
  int index) : _process(process), _parent(parent), _index(index), _loaded(false)
{
}

ProcessItem::~ProcessItem()
{
  while (!_next.isEmpty())
  {
    delete _next.takeFirst();
  }
}

Process ProcessItem::process() const
{
  return _process;
}

int ProcessItem::index() const
{
  return _index;
}

ProcessItem * ProcessItem::next(int index) const
{
  if (!_loaded && process().isValid())
  {
    _load();
  }
  return _next.at(index);
}

int ProcessItem::count() const
{
  if (!_loaded && process().isValid())
  {
    _load();
  }
  return _next.count();
}

bool ProcessItem::isLoaded() const
{
  return _loaded;
}

const ProcessItem * ProcessItem::_parentItem() const
{
  return _parent;
}

void ProcessItem::_load() const
{
  _loaded = true;
}

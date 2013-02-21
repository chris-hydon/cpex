#ifndef PROCESSITEM_H
#define PROCESSITEM_H

#include "model/process.h"

class ProcessItem
{
public:
  virtual ~ProcessItem();
  virtual Process process() const;
  virtual int index() const;
  ProcessItem * next(int) const;
  int count() const;
  bool isLoaded() const;

protected:
  ProcessItem(const Process &, const ProcessItem *, int);
  const ProcessItem * _parentItem() const;
  virtual void _load() const;

  const Process _process;
  const ProcessItem * _parent;
  const int _index;
  mutable QList<ProcessItem *> _next;
  mutable bool _loaded;
};

#endif // PROCESSITEM_H

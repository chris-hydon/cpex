#ifndef TRANSITIONMODEL_H
#define TRANSITIONMODEL_H

#include <QAbstractItemModel>
#include "process.h"
#include "processitem.h"

class TransitionItem : public ProcessItem
{
public:
  TransitionItem(const Process &, const TransitionItem *, const Event &, int, bool);
  // Overloaded constructor - intended for use only by the root, this constructs
  // two TransitionItems, one the parent of the next, so that the root may be
  // visible.
  TransitionItem(const Process &, bool);
  Event cause() const;
  const TransitionItem * parent() const;

protected:
  virtual void _load() const;

private:
  Event _cause;
};

class TransitionModel : public QAbstractItemModel
{
public:
  TransitionModel(const Process &, bool, QObject * parent = 0);
  ~TransitionModel();
  QModelIndex index(int, int, const QModelIndex & parent = QModelIndex()) const;
  QModelIndex parent(const QModelIndex &) const;
  int rowCount(const QModelIndex & parent = QModelIndex()) const;
  int columnCount(const QModelIndex & parent = QModelIndex()) const;
  QVariant data(const QModelIndex &, int role = Qt::DisplayRole) const;
  bool hasChildren(const QModelIndex & parent = QModelIndex()) const;

private:
  TransitionItem * _rootProcess;
  bool _asyncSemantics;
};

#endif // TRANSITIONMODEL_H

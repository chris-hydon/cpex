#ifndef TRANSITIONMODEL_H
#define TRANSITIONMODEL_H

#include "process.h"
#include <QAbstractItemModel>

class TransitionItem
{
public:
  TransitionItem(const Process &, const TransitionItem *, const Event &, int);
  // Overloaded constructor - intended for use only by the root, this constructs
  // two ProcessItems, one the parent of the next, so that the root may be visible.
  TransitionItem(const Process &);
  ~TransitionItem();
  TransitionItem * next(int) const;
  int count() const;
  bool canFetchMore() const;
  void fetchMore(int);

  const Process process;
  const TransitionItem * parent;
  const Event cause;
  const int index;

private:
  QList<TransitionItem *> _next;
};

class TransitionModel : public QAbstractItemModel
{
public:
  TransitionModel(const Process &, QObject * parent = 0);
  ~TransitionModel();
  QModelIndex index(int, int, const QModelIndex & parent = QModelIndex()) const;
  QModelIndex parent(const QModelIndex &) const;
  int rowCount(const QModelIndex & parent = QModelIndex()) const;
  int columnCount(const QModelIndex & parent = QModelIndex()) const;
  QVariant data(const QModelIndex &, int role = Qt::DisplayRole) const;
  bool canFetchMore(const QModelIndex &) const;
  void fetchMore(const QModelIndex &);
  bool hasChildren(const QModelIndex & parent = QModelIndex()) const;

private:
  TransitionItem * _rootProcess;
};

#endif // TRANSITIONMODEL_H

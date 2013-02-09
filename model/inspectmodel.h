#ifndef INSPECTMODEL_H
#define INSPECTMODEL_H

#include <QAbstractItemModel>
#include "model/process.h"

class InspectItem
{
public:
  InspectItem(const Process &, const InspectItem *, int);
  // Overloaded constructor - intended for use only by the root, this constructs
  // two InspectItems, one the parent of the next, so that the root may be visible.
  InspectItem(const Process &);
  ~InspectItem();
  InspectItem * next(int) const;
  int count() const;

  const Process process;
  const InspectItem * parent;
  const int index;

private:
  mutable QList<InspectItem *> _next;
  mutable bool _loaded;
};

class InspectModel : public QAbstractItemModel
{
public:
  InspectModel(const Process &, QObject * parent = 0);
  ~InspectModel();
  QModelIndex index(int, int, const QModelIndex & parent = QModelIndex()) const;
  QModelIndex parent(const QModelIndex &) const;
  int rowCount(const QModelIndex & parent = QModelIndex()) const;
  int columnCount(const QModelIndex & parent = QModelIndex()) const;
  QVariant data(const QModelIndex &, int role = Qt::DisplayRole) const;
  bool hasChildren(const QModelIndex & parent = QModelIndex()) const;

private:
  InspectItem * _rootProcess;
};

#endif // INSPECTMODEL_H

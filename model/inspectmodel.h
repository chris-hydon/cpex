#ifndef INSPECTMODEL_H
#define INSPECTMODEL_H

#include <QAbstractItemModel>
#include "model/process.h"
#include "model/processitem.h"

class InspectItem : public ProcessItem
{
public:
  InspectItem(const Process &, const InspectItem *, int);
  // Overloaded constructor - intended for use only by the root, this constructs
  // two InspectItems, one the parent of the next, so that the root may be visible.
  InspectItem(const Process &);
  const InspectItem * parent() const;

protected:
  void _load() const;
};

class InspectModel : public QAbstractItemModel
{
  Q_OBJECT

public:
  InspectModel(const Process &, QObject * parent = 0);
  ~InspectModel();
  QModelIndex index(int, int, const QModelIndex & parent = QModelIndex()) const;
  QModelIndex parent(const QModelIndex &) const;
  int rowCount(const QModelIndex & parent = QModelIndex()) const;
  int columnCount(const QModelIndex & parent = QModelIndex()) const;
  QVariant data(const QModelIndex &, int role = Qt::DisplayRole) const;
  bool hasChildren(const QModelIndex & parent = QModelIndex()) const;

signals:
  void eventChanged(const Event &);

public slots:
  void eventTextChanged(const QString &);

private:
  void _dataChanged(const QModelIndex &);

  InspectItem * _rootProcess;
  Event _event;
};

#endif // INSPECTMODEL_H

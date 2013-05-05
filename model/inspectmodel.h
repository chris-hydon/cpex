#ifndef INSPECTMODEL_H
#define INSPECTMODEL_H

#include <QAbstractItemModel>
#include "model/process.h"
#include "model/processitem.h"

class InspectItem : public ProcessItem
{
public:
  InspectItem(const Process &, const InspectItem *, int, bool);
  // Overloaded constructor - intended for use only by the root, this constructs
  // two InspectItems, one the parent of the next, so that the root may be visible.
  InspectItem(const Process &, bool);
  const InspectItem * parent() const;
  QList<Event> events() const;
  void setEvents(QList<Event> events);
  QVariant decoration() const;

protected:
  void _load() const;

private:
  // This is a list of events required by the event query component. While the
  // query itself is always a single event, renaming can cause it to become a list
  // of events, joined by "or".
  QList<Event> _events;

  // This is the decoration to be used by the model. Set when _events changes.
  QVariant _deco;
};

class InspectModel : public QAbstractItemModel
{
  Q_OBJECT

public:
  InspectModel(const Process &, bool, QObject * parent = 0);
  ~InspectModel();
  QModelIndex index(int, int, const QModelIndex & parent = QModelIndex()) const;
  QModelIndex parent(const QModelIndex &) const;
  int rowCount(const QModelIndex & parent = QModelIndex()) const;
  int columnCount(const QModelIndex & parent = QModelIndex()) const;
  QVariant data(const QModelIndex &, int role = Qt::DisplayRole) const;
  bool hasChildren(const QModelIndex & parent = QModelIndex()) const;

signals:
  void eventChanged();

public slots:
  void eventTextChanged(const QString &);
  void refreshData(const QModelIndex &);

private:
  void _dataChanged(const QModelIndex &, QList<Event>);

  InspectItem * _rootProcess;
  bool _asyncSemantics;
};

#endif // INSPECTMODEL_H

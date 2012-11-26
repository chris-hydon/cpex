#ifndef PROCESSMODEL_H
#define PROCESSMODEL_H

#include "process.h"
#include <QAbstractItemModel>

class ProcessItem
{
public:
  ProcessItem(const Process & process, const ProcessItem * parent = NULL,
    const Event & cause = Event(), int index = 0);
  ~ProcessItem();
  ProcessItem * next(int index) const;
  int count() const;
  bool canFetchMore() const;
  void fetchMore(int toFetch);

  const Process process;
  const ProcessItem * parent;
  const Event cause;
  const int index;

private:
  QList<ProcessItem *> _next;
};

class ProcessModel : public QAbstractItemModel
{
public:
  ProcessModel(const Process & rootProcess, QObject * parent = 0);
  ~ProcessModel();
  QModelIndex index(int row, int column, const QModelIndex & parent = QModelIndex()) const;
  QModelIndex parent(const QModelIndex & index) const;
  int rowCount(const QModelIndex & parent = QModelIndex()) const;
  int columnCount(const QModelIndex & parent = QModelIndex()) const;
  QVariant data(const QModelIndex & index, int role = Qt::DisplayRole) const;
  bool canFetchMore(const QModelIndex & parent) const;
  void fetchMore(const QModelIndex & parent);
  bool hasChildren(const QModelIndex & parent = QModelIndex()) const;

private:
  ProcessItem * _rootProcess;
};

#endif // PROCESSMODEL_H

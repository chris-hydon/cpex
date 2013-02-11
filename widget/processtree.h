#ifndef PROCESSTREE_H
#define PROCESSTREE_H

#include <QAbstractItemModel>
#include <QTreeView>
#include "cspmsession.h"

class ProcessTree : public QTreeView
{
  Q_OBJECT
public:
  explicit ProcessTree(const CSPMSession *, QWidget * parent = 0);

signals:
  void itemSelected(const QModelIndex & index);

public slots:
  void showContextMenu(const QPoint &);

protected slots:
  void selectionChanged();

private:
  QAbstractItemModel * _model;
  const CSPMSession * _session;
};

#endif // PROCESSTREE_H

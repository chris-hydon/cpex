#ifndef PROCESSTREE_H
#define PROCESSTREE_H

#include <QTreeView>

class ProcessTree : public QTreeView
{
  Q_OBJECT
public:
  explicit ProcessTree(QWidget *parent = 0);
  
signals:
  
public slots:
  void loadInitialState();
};

#endif // PROCESSTREE_H

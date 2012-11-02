#ifndef TRACELISTWIDGET_H
#define TRACELISTWIDGET_H

#include <QListView>
#include <QStringListModel>

class TraceListWidget : public QListView
{
  Q_OBJECT
public:
  explicit TraceListWidget(QWidget * parent = 0);

signals:

public slots:
  void setTraces(const QModelIndex & index);

private:
  QStringListModel * _model;
};

#endif // TRACELISTWIDGET_H

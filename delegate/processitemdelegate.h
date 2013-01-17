#ifndef PROCESSITEMDELEGATE_H
#define PROCESSITEMDELEGATE_H

#include <QStyledItemDelegate>

class ProcessItemDelegate : public QStyledItemDelegate
{
  Q_OBJECT
public:
  explicit ProcessItemDelegate(QObject * parent = 0);
  virtual void paint(QPainter * painter, const QStyleOptionViewItem & option, const QModelIndex & index) const;

private:
  QString stylize(QString input) const;

};

#endif // PROCESSITEMDELEGATE_H

#ifndef PROCESSITEMDELEGATE_H
#define PROCESSITEMDELEGATE_H

#include <QStyledItemDelegate>

class ProcessItemDelegate : public QStyledItemDelegate
{
  Q_OBJECT
public:
  explicit ProcessItemDelegate(QObject * parent = 0);
  virtual void paint(QPainter *, const QStyleOptionViewItem &,
    const QModelIndex &) const;
  QSize sizeHint(const QStyleOptionViewItem &, const QModelIndex &) const;

private:
  QString stylize(QString) const;

};

#endif // PROCESSITEMDELEGATE_H

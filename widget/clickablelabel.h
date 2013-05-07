#ifndef CLICKABLELABEL_H
#define CLICKABLELABEL_H

#include <QLabel>

class ClickableLabel : public QLabel
{
  Q_OBJECT
public:
  explicit ClickableLabel(QWidget * = 0);

signals:
  void clicked();

protected:
  virtual void mousePressEvent(QMouseEvent *);
};

#endif // CLICKABLELABEL_H

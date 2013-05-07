#include "clickablelabel.h"

#include <QMouseEvent>

ClickableLabel::ClickableLabel(QWidget * parent) :
  QLabel(parent)
{
}

void ClickableLabel::mousePressEvent(QMouseEvent * event)
{
  if (event->buttons() == Qt::LeftButton)
  {
    emit clicked();
  }
}

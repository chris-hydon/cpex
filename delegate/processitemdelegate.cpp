#include "processitemdelegate.h"

#include <QApplication>
#include <QPainter>
#include <QTextDocument>

ProcessItemDelegate::ProcessItemDelegate(QObject * parent) :
  QStyledItemDelegate(parent)
{
}

void ProcessItemDelegate::paint(QPainter * painter,
  const QStyleOptionViewItem &option, const QModelIndex & index) const
{
  QStyleOptionViewItemV4 opt = option;
  initStyleOption(&opt, index);

  painter->save();
  painter->setClipRect(opt.rect);

  // Draw the background.
  const QWidget * widget = opt.widget;
  QStyle * style = widget ? widget->style() : QApplication::style();
  style->proxy()->drawPrimitive(QStyle::PE_PanelItemViewItem, &opt, painter, widget);

  // Draw the text.
  QTextDocument doc;
  doc.setHtml(opt.text);
  doc.setDocumentMargin(2);
  painter->translate(opt.rect.topLeft());
  doc.drawContents(painter);

  painter->restore();
}

QSize ProcessItemDelegate::sizeHint(const QStyleOptionViewItem & option,
  const QModelIndex & index) const
{
  QStyleOptionViewItemV4 opt = option;
  initStyleOption(&opt, index);

  QTextDocument doc;
  doc.setHtml(opt.text);
  doc.setDocumentMargin(2);
  return QSize(doc.size().width(), doc.size().height());
}

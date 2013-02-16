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

  QRect iconRect = style->subElementRect(QStyle::SE_ItemViewItemDecoration, &opt,
    widget);
  QRect textRect = style->subElementRect(QStyle::SE_ItemViewItemText, &opt, widget);

  // Draw the icon.
  QIcon::Mode mode = QIcon::Normal;
  if (!(opt.state & QStyle::State_Enabled))
  {
    mode = QIcon::Disabled;
  }
  else if (opt.state & QStyle::State_Selected)
  {
    mode = QIcon::Selected;
  }
  QIcon::State state = opt.state & QStyle::State_Open ? QIcon::On : QIcon::Off;
  opt.icon.paint(painter, iconRect, opt.decorationAlignment, mode, state);

  // Draw the text.
  QTextDocument doc;
  doc.setHtml(opt.text);
  doc.setDocumentMargin(2);
  painter->translate(textRect.topLeft());
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
  quint32 width = doc.size().width() + opt.decorationSize.width() + 6;
  quint32 height = qMax((int) doc.size().height(), opt.decorationSize.height());
  return QSize(width, height);
}

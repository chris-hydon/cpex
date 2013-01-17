#include "processitemdelegate.h"

#include <QPainter>
#include <QTextDocument>

#include <QApplication>

ProcessItemDelegate::ProcessItemDelegate(QObject * parent) :
  QStyledItemDelegate(parent)
{
}

const QString stylesheet = QString("<style type=\"text/css\">") +
  ".brac0 { color: #c00; }" +
  ".brac1 { color: #00c; }" +
  ".brac2 { color: #0c0; }" +
  ".brac3 { color: #990; }" +
  ".brac4 { color: #909; }" +
  ".brac5 { color: #099; }" +
  "</style>";

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
  doc.setHtml(stylesheet + stylize(index.data().toString()));
  doc.setDocumentMargin(2);
  painter->translate(opt.rect.topLeft());
  doc.drawContents(painter);

  painter->restore();
}

QString ProcessItemDelegate::stylize(QString input) const
{
  // Match any CSPM open or close bracket. This is a little unclear here because
  // of all the backslashes, but matches (, [, [|, [[, { and their respective closing
  // brackets. |> also counts as a closing bracket ("[|a|>" is CSPM for exception on
  // events a). [> is also matched, and is a special case: neither opening nor
  // closing.
  QRegExp brackets("(\\(|\\[[\\[|>]?|\\{|\\}|[\\]|]?\\]|\\)|\\|>)");
  QStringList open;
  open << "(" << "[" << "{" << "[[" << "[|";
  QString span("<span class=\"brac%1\">%2</span>");
  int i = 0;
  int level = 0;
  while ((i = brackets.indexIn(input, i)) != -1)
  {
    QString m = brackets.capturedTexts()[0];
    bool isOpen = open.contains(m);
    if (!isOpen && m != "[>") level--;

    input.replace(i, brackets.matchedLength(), span.arg(QString::number(level % 6), m));
    i += 27 + brackets.matchedLength(); // Length of QString span after args are replaced.

    if (isOpen) level++;
  }

  return input;
}

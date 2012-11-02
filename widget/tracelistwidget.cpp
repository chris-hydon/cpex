#include "tracelistwidget.h"

#include "model/process.h"

TraceListWidget::TraceListWidget(QWidget * parent) : QListView(parent)
{
  _model = new QStringListModel();
  setModel(_model);
}

void TraceListWidget::setTraces(const QModelIndex & index)
{
  const Process * proc = static_cast<Process *>(index.internalPointer());
  QStringList labels;
  while (proc->parent() != NULL)
  {
    if (proc->causedBy()->type() != Event::Tau)
    {
      labels.prepend(proc->causedBy()->displayText());
    }
    proc = proc->parent();
  }

  _model->setStringList(labels);
}

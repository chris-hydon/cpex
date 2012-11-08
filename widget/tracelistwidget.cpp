#include "tracelistwidget.h"

#include "model/process.h"
#include "model/processmodel.h"

TraceListWidget::TraceListWidget(QWidget * parent) : QListView(parent)
{
  _model = new QStringListModel();
  setModel(_model);
}

void TraceListWidget::setTraces(const QModelIndex & index)
{
  const ProcessItem * proc = static_cast<ProcessItem *>(index.internalPointer());
  QStringList labels;
  while (proc->parent != NULL)
  {
    if (proc->cause->type() != Event::Tau)
    {
      labels.prepend(proc->cause->displayText());
    }
    proc = proc->parent;
  }

  _model->setStringList(labels);
}

#include "errordialog.h"

#include <QGridLayout>
#include <QHeaderView>
#include <QPushButton>
#include "programstate.h"

ErrorDialog::ErrorDialog(QWidget * parent) : QDialog(parent)
{
  setWindowTitle(tr("Error log"));
  resize(800, 400);

  QGridLayout * grid = new QGridLayout(this);

  QStringList headers;
  headers << tr("Session");
  headers << tr("Message");

  uiTable = new QTableWidget(0, 2, this);
  uiTable->setHorizontalHeaderLabels(headers);
  uiTable->setSelectionBehavior(QAbstractItemView::SelectRows);
  uiTable->setEditTriggers(QAbstractItemView::NoEditTriggers);
  uiTable->verticalHeader()->setVisible(false);
  uiTable->setTextElideMode(Qt::ElideNone);
  uiTable->horizontalHeader()->setStretchLastSection(true);

  QPushButton * close = new QPushButton("Close", this);
  close->setDefault(true);
  connect(close, SIGNAL(clicked()), this, SLOT(close()));

  grid->addWidget(uiTable, 0, 0);
  grid->addWidget(close, 1, 0, Qt::AlignRight);
}

void ErrorDialog::errorsChanged()
{
  static QString tError = tr("Error: %1");
  static QString tWarning = tr("Warning: %1");

  QList<CSPError *> errors = ProgramState::getErrors();
  int knownCount = _errors.count();
  int newCount = errors.count();
  uiTable->setRowCount(newCount);
  for (int i = 0; i < newCount; i++)
  {
    if (knownCount > i)
    {
      // Don't bother changing the view for what hasn't changed.
      if (errors[i] == _errors[i])
      {
        continue;
      }
      _errors.replace(i, errors[i]);
    }
    else
    {
      _errors.append(errors[i]);
    }

    uiTable->setItem(i, 0, new QTableWidgetItem(errors[i]->sessionName()));
    uiTable->setItem(i, 1, new QTableWidgetItem(
      errors[i]->type() == CSPError::Error ? tError.arg(errors[i]->message())
      : tWarning.arg(errors[i]->message())));
    uiTable->resizeRowToContents(i);
  }

  // Truncate the list. Don't delete any pointers here, ProgramState should be
  // responsible for whether or not a CSPError * is valid.
  if (newCount < knownCount)
  {
    _errors = _errors.mid(0, newCount);
  }
}

#ifndef ERRORDIALOG_H
#define ERRORDIALOG_H

#include <QDialog>
#include <QList>
#include <QTableWidget>
#include "csperror.h"

class ErrorDialog : public QDialog
{
  Q_OBJECT
public:
  explicit ErrorDialog(QWidget * = 0);

public slots:
  void errorsChanged();

private:
  QList<CSPError *> _errors;
  QTableWidget * uiTable;
};

#endif // ERRORDIALOG_H

#ifndef SUMMARYEDITOR_H
#define SUMMARYEDITOR_H

#include <QWidget>
#include "ui_summaryeditor.h"

class SummaryEditor : public QDialog
{
  Q_OBJECT

public:
  SummaryEditor(QWidget *parent = 0);
  ~SummaryEditor();

  Ui::summaryDialog ui;

private slots:

private:

};

#endif // SUMMARYEDITOR_H

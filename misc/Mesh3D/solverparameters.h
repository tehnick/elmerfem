#ifndef SOLVERPARAMETERS_H
#define SOLVERPARAMETERS_H

#include <QWidget>
#include "dynamiceditor.h"
#include "ui_solverparameters.h"

class SolverParameterEditor : public QDialog
{
  Q_OBJECT

public:
  SolverParameterEditor(QWidget *parent = 0);
  ~SolverParameterEditor();

  DynamicEditor *generalOptions;
  Ui::solverParameterEditor ui;

signals:

private slots:

private:

};

#endif // SOLVERPARAMETERS_H

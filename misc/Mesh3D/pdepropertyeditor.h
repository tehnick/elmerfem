#ifndef PDEPROPERTYEDITOR_H
#define PDEPROPERTYEDITOR_H

#include <QWidget>
#include "ui_pdepropertyeditor.h"
#include "maxlimits.h"
#include "solverparameters.h"

class PDEPropertyEditor : public QDialog
{
  Q_OBJECT

public:
  PDEPropertyEditor(QWidget *parent = 0);
  ~PDEPropertyEditor();

  Ui::equationEditor ui;

  SolverParameterEditor *solverParameterEditor;

  void startEdit(int);
  void defaultSettings();

  QAction *menuAction;

signals:
  void signalPdeEditorFinished(int, int);

private slots:
  void acceptButtonClicked();
  void deleteButtonClicked();
  void editNumericalMethods();

private:
  int myId;
};

#endif // PDEPROPERTYEDITOR_H

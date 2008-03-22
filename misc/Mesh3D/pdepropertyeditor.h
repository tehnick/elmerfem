#ifndef PDEPROPERTYEDITOR_H
#define PDEPROPERTYEDITOR_H

#include <QWidget>
#include "ui_pdepropertyeditor.h"

class PDEPropertyEditor : public QDialog
{
  Q_OBJECT

public:
  PDEPropertyEditor(QWidget *parent = 0);
  ~PDEPropertyEditor();

  Ui::equationEditor ui;

  void startEdit(int);

  QAction *menuAction;

signals:
  void signalPdeEditorFinished(int, int);

private slots:
  void acceptButtonClicked();
  void deleteButtonClicked();

private:
  int myId;
};

#endif // PDEPROPERTYEDITOR_H

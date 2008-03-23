#ifndef MATPROPERTYEDITOR_H
#define MATPROPERTYEDITOR_H

#include <QWidget>
#include "ui_matpropertyeditor.h"

class MATPropertyEditor : public QDialog
{
  Q_OBJECT

public:
  MATPropertyEditor(QWidget *parent = 0);
  ~MATPropertyEditor();

  Ui::materialEditor ui;

  void startEdit(int);
  void defaultSettings();

  QAction *menuAction;

signals:
  void signalMatEditorFinished(int, int);

private slots:
  void acceptButtonClicked();
  void deleteButtonClicked();

private:
  int myId;
};

#endif // MATPROPERTYEDITOR_H

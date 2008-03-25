#ifndef BODYPROPERTYEDITOR_H
#define BODYPROPERTYEDITOR_H

#include <QWidget>
#include "ui_bodypropertyeditor.h"

#define MAX_BODIES 100

class BodyPropertyEditor : public QDialog
{
  Q_OBJECT

public:
  BodyPropertyEditor(QWidget *parent = 0);
  ~BodyPropertyEditor();

  Ui::bodyPropertyDialog ui;

  bool touched;

private slots:
  void applySlot();
  void discardSlot();

private:

};

#endif // BODYPROPERTYEDITOR_H

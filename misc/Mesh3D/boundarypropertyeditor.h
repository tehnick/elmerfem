#ifndef BOUNDARYPROPERTYEDITOR_H
#define BOUNDARYPROPERTYEDITOR_H

#include <QWidget>
#include "ui_boundarypropertyeditor.h"

class BoundaryPropertyEditor : public QDialog
{
  Q_OBJECT

public:
  BoundaryPropertyEditor(QWidget *parent = 0);
  ~BoundaryPropertyEditor();

  Ui::boundaryPropertyDialog ui;

  bool touched;

private slots:
  void applySlot();
  void discardSlot();

private:

};

#endif // BOUNDARYPROPERTYEDITOR_H

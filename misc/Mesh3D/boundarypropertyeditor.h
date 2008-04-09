#ifndef BOUNDARYPROPERTYEDITOR_H
#define BOUNDARYPROPERTYEDITOR_H

#include <QWidget>
#include "dynamiceditor.h"
#include "ui_boundarypropertyeditor.h"

class BoundaryPropertyEditor : public QDialog
{
  Q_OBJECT

signals:
  void BoundaryComboChanged(BoundaryPropertyEditor *, QString);

public:
  BoundaryPropertyEditor(QWidget *parent = 0);
  ~BoundaryPropertyEditor();


  Ui::boundaryPropertyDialog ui;
  DynamicEditor *condition;

  bool touched;

public slots:
  void boundaryComboChanged(QString);

private slots:
  void applySlot();
  void discardSlot();

private:

};

#endif // BOUNDARYPROPERTYEDITOR_H

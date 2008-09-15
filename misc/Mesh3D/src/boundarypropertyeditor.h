#ifndef BOUNDARYPROPERTYEDITOR_H
#define BOUNDARYPROPERTYEDITOR_H

#include <QWidget>
#include "dynamiceditor.h"
#include "bodypropertyeditor.h"
#include "ui_boundarypropertyeditor.h"

class BoundaryPropertyEditor : public QDialog
{
  Q_OBJECT

signals:
  void BoundaryAsABodyChanged(BoundaryPropertyEditor *, int);
  void BoundaryComboChanged(BoundaryPropertyEditor *, QString);

public:
  BoundaryPropertyEditor(QWidget *parent = 0);
  ~BoundaryPropertyEditor();


  Ui::boundaryPropertyDialog ui;
  DynamicEditor *condition;

  int bodyID;
  BodyPropertyEditor *bodyProperties;

  bool touched;

public slots:
  void boundaryAsABodyChanged(int);
  void boundaryComboChanged(QString);

private slots:
  void applySlot();
  void discardSlot();

private:

};

#endif // BOUNDARYPROPERTYEDITOR_H

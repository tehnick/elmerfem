#ifndef BOUNDARYDIVISION_H
#define BOUNDARYDIVISION_H

#include "ui_boundarydivision.h"

class BoundaryDivide: public QDialog
{
  Q_OBJECT
    
public:
  BoundaryDivide(QWidget *parent = 0);
  ~BoundaryDivide();

  QString angleDegree;
  Ui::BoundaryDivisionForm ui;

signals:
  void signalDoDivision(double);

private slots:
  void defineAngle(const QString &qs);
  void divideBoundary();

};

#endif

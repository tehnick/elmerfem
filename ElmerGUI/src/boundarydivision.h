#ifndef BOUNDARYDIVISION_H
#define BOUNDARYDIVISION_H

#include "ui_boundarydivision.h"

#define TARGET_UNKNOWN  0
#define TARGET_SURFACES 1
#define TARGET_EDGES    2

class BoundaryDivide: public QDialog
{
  Q_OBJECT
    
public:
  BoundaryDivide(QWidget *parent = 0);
  ~BoundaryDivide();

  QString angleDegree;
  Ui::BoundaryDivisionForm ui;

  int target; 

signals:
  void signalDoDivideSurface(double);
  void signalDoDivideEdge(double);

private slots:
  void defineAngle(const QString &qs);
  void divideBoundary();

};

#endif

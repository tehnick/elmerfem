#ifndef BOUNDARYDIVISION_H
#define BOUNDARYDIVISION_H

#include "ui_boundarydivision.h"

enum TargetTypes {
  TARGET_UNKNOWN,
  TARGET_SURFACES,
  TARGET_EDGES
};

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

#include <QtGui>
#include <iostream>
#include "boundarydivision.h"

BoundaryDivide::BoundaryDivide(QWidget *parent)
  : QDialog(parent)
{
  ui.setupUi(this);

  connect(ui.angleDegreeEdit, SIGNAL(textChanged(const QString&)), this, SLOT(defineAngle(const QString&)));
  connect(ui.divideButton, SIGNAL(clicked()), this, SLOT(divideBoundary()));
  connect(ui.closeButton, SIGNAL(clicked()), this, SLOT(close()));

  ui.angleDegreeEdit->setText("20.0");
}

BoundaryDivide::~BoundaryDivide()
{
}

void BoundaryDivide::defineAngle(const QString &qs)
{
  angleDegree = qs;
}

void BoundaryDivide::divideBoundary()
{
  double angle = angleDegree.toDouble();
  emit(signalDoDivision(angle));
}

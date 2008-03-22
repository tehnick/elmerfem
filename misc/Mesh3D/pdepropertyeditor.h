#ifndef PDEPROPERTYEDITOR_H
#define PDEPROPERTYEDITOR_H

#include <QWidget>
#include "ui_pdepropertyeditor.h"

#define MAX_EQUATIONS 1000

class heatEquation_t {
 public:
  bool active;
  bool convectionNone;
  bool convectionConstant;
  bool convectionComputed;
  bool phaseChangeNone;
  bool phaseChangeSpatial1;
  bool phaseChangeSpatial2;
  bool phaseChangeTemporal;
  bool latentHeatRelease;
};

class linearElasticity_t {
 public:
  bool active;
  bool planeStress;
};

class navierStokes_t {
 public:
  bool active;
  bool calculateHydrostaticPressure;
  bool turbulenceModelNone;
  bool turbulenceModelKE;
  double keClip;
};

class advectionDiffusion_t {
 public:
  bool active;
  bool convectionNone;
  bool convectionConstant;
  bool convectionComputed;
};

class helmholtzEquation_t {
 public:
  bool active;
  double angularFrequency;
};

class equation_t {
 public:
  QString               equationName;
  heatEquation_t        heatEquation;
  linearElasticity_t    linearElasticity;
  navierStokes_t        navierStokes;
  advectionDiffusion_t  advectionDiffusion;
  helmholtzEquation_t   helmholtzEquation;
};

class PDEPropertyEditor : public QDialog
{
  Q_OBJECT

public:
  PDEPropertyEditor(QWidget *parent = 0);
  ~PDEPropertyEditor();

  Ui::equationEditor ui;

  int equations;
  equation_t *equation;

private slots:
  void acceptButtonClicked();
  void deleteButtonClicked();

private:

};

#endif // PDEPROPERTYEDITOR_H

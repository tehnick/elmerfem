#ifndef MESHCONTROL_H
#define MESHCONTROL_H

#include "meshtype.h"
#include "ui_meshcontrol.h"

class MeshControl : public QDialog
{
  Q_OBJECT
    
public:
  MeshControl(QWidget *parent = 0);
  ~MeshControl();

  int generatorType;
  QString tetlibControlString;
  QString nglibMaxH;
  QString nglibFineness;
  QString nglibBackgroundmesh;
  QString elmerGridControlString;

  Ui::MeshcontrolForm ui;

  bool tetlibPresent;
  bool nglibPresent;

public slots:
  void defaultControls();

private slots:
  void tetlibClicked();
  void nglibClicked(); 
  void elmerGridClicked();
  void defineTetlibControlString(const QString &qs);
  void defineNglibMaxH(const QString &qs);
  void defineNglibFineness(const QString &qs);
  void defineNglibBackgroundmesh(const QString &qs);
  void defineElmerGridControlString(const QString &qs);

};

#endif

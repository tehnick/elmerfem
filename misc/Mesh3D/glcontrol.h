#ifndef GLCONTROL_H
#define GLCONTROL_H

#include <QWidget>
#include "ui_glcontrol.h"
#include "glwidget.h"
#include "renderarea.h"

class GLcontrol : public QDialog
{
  Q_OBJECT

public:
  GLcontrol(QWidget *parent = 0);
  ~GLcontrol();

  Ui::glControlDialog ui;

  GLWidget *glWidget;

  GLfloat ambient;
  GLfloat diffuse;
  GLfloat specular;
  GLfloat posx;
  GLfloat posy;
  GLfloat posz;
  GLfloat matAmbient;
  GLfloat matDiffuse;
  GLfloat matSpecular;
  GLfloat matShininess;

signals:

private slots:
  void okButtonClicked();

private:
 
};

#endif // GLCONTROL_H

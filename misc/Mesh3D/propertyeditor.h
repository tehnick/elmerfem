#ifndef PROPERTYEDITOR_H
#define PROPERTYEDITOR_H

#define MAX_BCS 100000

#include <QWidget>

#include "ui_propertyeditor.h"

class QTextEdit;
class QPushButton;
class QMenu;
class QTabWidget;

class bcProperty_t {
 public:
  bool defined;
  QString temperature;
  QString heatFlux;
  QString displacement1;
  QString displacement2;
  QString displacement3;
};

class PropertyEditor : public QDialog
{
  Q_OBJECT

public:
  PropertyEditor(QWidget *parent = 0);
  ~PropertyEditor();

  bcProperty_t bcProperty[MAX_BCS];

  bool heatEquationActive;
  bool linearElasticityActive;

  void editProperties(int);

  Ui::bcPropertyDialog ui;

  int maxindex;

private slots:
  void temperatureChanged(const QString&);
  void heatFluxChanged(const QString&);
  void displacement1Changed(const QString&);
  void displacement2Changed(const QString&);
  void displacement3Changed(const QString&);

private:
  int bcIndex;

};

#endif // PROPERTYEDITOR_H

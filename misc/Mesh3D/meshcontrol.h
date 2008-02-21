#ifndef MESHCONTROL_H
#define MESHCONTROL_H

#include <QtGui>
#include <QWidget>

class QPushButton;

class MeshControl : public QWidget
{
  Q_OBJECT

public:
  MeshControl(QWidget *parent = 0);
  ~MeshControl();

  QString tetgenControlString;

private slots:
  void defineControlString(const QString &qs);
  void defaultControlString();

private:
  QLineEdit *meshControlEdit;
};

#endif

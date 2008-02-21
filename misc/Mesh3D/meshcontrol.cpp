#include <QtGui>
#include <iostream>
#include "meshcontrol.h"

MeshControl::MeshControl(QWidget *parent)
  : QWidget(parent)
{
  setWindowFlags(Qt::Drawer);

  QLabel *meshControlLabel = new QLabel(tr("tetlib control string:"));
  meshControlEdit = new QLineEdit(this);
  connect(meshControlEdit, SIGNAL(textChanged(const QString&)), this, SLOT(defineControlString(const QString&)));

  QPushButton *clearButton = new QPushButton(tr("&Default"));
  connect(clearButton, SIGNAL(clicked()), this, SLOT(defaultControlString()));
  
  QPushButton *closeButton = new QPushButton(tr("&Close"));
  connect(closeButton, SIGNAL(clicked()), this, SLOT(close()));

  QVBoxLayout *layout = new QVBoxLayout;

  layout->addWidget(meshControlLabel);
  layout->addWidget(meshControlEdit);
  layout->addWidget(clearButton);
  layout->addWidget(closeButton);
  setLayout(layout);
  
  defaultControlString();

  setWindowTitle(tr("Mesh control"));
}

MeshControl::~MeshControl()
{
}

void MeshControl::defineControlString(const QString &qs)
{
  tetgenControlString = qs;
}

void MeshControl::defaultControlString()
{
  meshControlEdit->setText("Jpq1.414V");  
}

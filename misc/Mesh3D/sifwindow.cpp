#include <QtGui>
#include <iostream>
#include "sifwindow.h"

SifWindow::SifWindow(QWidget *parent)
  : QWidget(parent)
{
  setWindowFlags(Qt::Drawer);

  textEdit = new QTextEdit;
  textEdit->setLineWrapMode(QTextEdit::NoWrap);

  clearButton = new QPushButton(tr("&Clear"));
  connect(clearButton, SIGNAL(clicked()), this, SLOT(clearSif()));

  closeButton = new QPushButton(tr("&Close"));
  connect(closeButton, SIGNAL(clicked()), this, SLOT(close()));
  
  QVBoxLayout *layout = new QVBoxLayout;
  layout->addWidget(textEdit);
  layout->addWidget(clearButton);
  layout->addWidget(closeButton);
  setLayout(layout);
  
  setWindowTitle(tr("Sif"));
}

SifWindow::~SifWindow()
{
}

QSize SifWindow::minimumSizeHint() const
{
  return QSize(64, 64);
}


QSize SifWindow::sizeHint() const
{
  return QSize(380, 480);
}

void SifWindow::clearSif()
{
  textEdit->clear();
}

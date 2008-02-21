#include <QtGui>
#include <iostream>
#include "logwindow.h"

LogWindow::LogWindow(QWidget *parent)
  : QWidget(parent)
{
  std::cout << "LogWindow::LogWindow\n";
  std::cout.flush();

  setWindowFlags(Qt::Drawer);

  textEdit = new QTextEdit;
  textEdit->setReadOnly(true);
  textEdit->setLineWrapMode(QTextEdit::NoWrap);

  closeButton = new QPushButton(tr("&Close"));
  connect(closeButton, SIGNAL(clicked()), this, SLOT(close()));
  
  QVBoxLayout *layout = new QVBoxLayout;
  layout->addWidget(textEdit);
  layout->addWidget(closeButton);
  setLayout(layout);
  
  setWindowTitle(tr("Log"));
}

LogWindow::~LogWindow()
{
  std::cout << "LogWindow::~LogWindow\n";
  std::cout.flush();
}

QSize LogWindow::minimumSizeHint() const
{
  return QSize(64, 64);
}


QSize LogWindow::sizeHint() const
{
  return QSize(380, 480);
}

#include <QApplication>

#include "mainwindow.h"
#include "glwidget.h"
#include "helpers.h"

int main(int argc, char *argv[])
{
  QApplication app(argc, argv);
  MainWindow mainWin;
  mainWin.show();
  return app.exec();
}

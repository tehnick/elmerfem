#ifndef TESTER_H
#define TESTER_H

#include <QtGui>
#include "ui_mainform.h"

class Tester : public QWidget
{
 Q_OBJECT

 public:
  Tester(QWidget *parent = 0);
  void testEnvironment();
  void testExecutables();
  void verdict();

 private:
  Ui::mainForm ui;
  bool kosher;
};

#endif // TESTER_H

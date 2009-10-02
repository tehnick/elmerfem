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
  QString get(const QString &variable);
  bool testDir(const QString &variable, QLabel *label);
  bool testFile(const QString &value, QLabel *label);
  bool testPath(const QString &value, QLabel *label);
  bool testLdLibraryPath(const QString &value, QLabel *label);
  void testFunctionality();

  Ui::mainForm ui;
  QString elmerHome;
  QString elmerGuiHome;
  bool ok;
  QProcess *solver;
  QProcess *post;
  QProcess *grid;

 private slots:
  void solverFinished(int exitCode, QProcess::ExitStatus exitStatus);
  void postFinished(int exitCode, QProcess::ExitStatus exitStatus);
  void gridFinished(int exitCode, QProcess::ExitStatus exitStatus);

};

#endif // TESTER_H

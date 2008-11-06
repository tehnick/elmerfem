/*****************************************************************************
 *                                                                           *
 *  Elmer, A Finite Element Software for Multiphysical Problems              *
 *                                                                           *
 *  Copyright 1st April 1995 - , CSC - Scientific Computing Ltd., Finland    *
 *                                                                           *
 *  This program is free software; you can redistribute it and/or            *
 *  modify it under the terms of the GNU General Public License              *
 *  as published by the Free Software Foundation; either version 2           *
 *  of the License, or (at your option) any later version.                   *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *
 *  GNU General Public License for more details.                             *
 *                                                                           *
 *  You should have received a copy of the GNU General Public License        *
 *  along with this program (in file fem/GPL-2); if not, write to the        *
 *  Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,         *
 *  Boston, MA 02110-1301, USA.                                              *
 *                                                                           *
 *****************************************************************************/

/*****************************************************************************
 *                                                                           *
 *  ElmerGUIlogger                                                           *
 *                                                                           *
 *****************************************************************************
 *                                                                           *
 *  Authors: Mikko Lyly, Juha Ruokolainen and Peter Råback                   *
 *  Email:   Juha.Ruokolainen@csc.fi                                         *
 *  Web:     http://www.csc.fi/elmer                                         *
 *  Address: CSC - Scientific Computing Ltd.                                 *
 *           Keilaranta 14                                                   *
 *           02101 Espoo, Finland                                            *
 *                                                                           *
 *  Original Date: 15 Mar 2008                                               *
 *                                                                           *
 *****************************************************************************/

#include <QAction>
#include <QMenu>
#include <QMenuBar>
#include <QTextEdit>
#include <QIcon>
#include <QColor>
#include "mainwindow.h"

MainWindow::MainWindow()
{
  setWindowTitle("ElmerGUI logger");
  setWindowIcon(QIcon(":/icons/ElmerGUI.png"));
  resize(400, 200);

  textEdit = new QTextEdit(this);
  setCentralWidget(textEdit);

  createActions();
  createMenus();

  QString ELMERGUI_HOME = getenv("ELMERGUI_HOME");
  textEdit->append("ELMERGUI_HOME = " + ELMERGUI_HOME);

  textEdit->append("Starting ElmerGUI...");

  elmerGUI = new QProcess(this);

  connect(elmerGUI, SIGNAL(error(QProcess::ProcessError)), this, SLOT(errorSlot(QProcess::ProcessError)));
  connect(elmerGUI, SIGNAL(finished(int, QProcess::ExitStatus)), this, SLOT(finishedSlot(int, QProcess::ExitStatus)));
  connect(elmerGUI, SIGNAL(readyReadStandardOutput()), this, SLOT(stdoutSlot()));
  connect(elmerGUI, SIGNAL(readyReadStandardError()), this, SLOT(stderrSlot()));
  connect(elmerGUI, SIGNAL(started()), this, SLOT(startedSlot()));
  connect(elmerGUI, SIGNAL(stateChanged(QProcess::ProcessState)), this, SLOT(stateChangedSlot(QProcess::ProcessState)));

  elmerGUI->start("ElmerGUI");

  if(!elmerGUI->waitForStarted()) {
    textEdit->append("Failed starting the process - aborting");
    return;
  }
}

MainWindow::~MainWindow()
{
}

void MainWindow::exitSlot()
{
  if(elmerGUI->state() == QProcess::Running) {

    elmerGUI->kill();

    if(!elmerGUI->waitForFinished())
      textEdit->append("Failed killing the process - closing anyways");
  }
  
  close();
}

void MainWindow::createActions()
{
  exitAct = new QAction(QIcon(), tr("Exit"), this);
  exitAct->setShortcut(tr("Ctrl+Q"));
  connect(exitAct, SIGNAL(triggered()), this, SLOT(exitSlot()));
}

void MainWindow::createMenus()
{
  fileMenu = menuBar()->addMenu(tr("&File"));
  fileMenu->addAction(exitAct);
}

void MainWindow::errorSlot(QProcess::ProcessError error)
{
  textEdit->setTextColor(Qt::blue);
  textEdit->append("Process error");

  switch(error) {
  case(QProcess::FailedToStart):
    textEdit->append("Error: Failed to start");
    break;
  case(QProcess::Crashed):
    textEdit->append("Error: Crashed");
    break;
  case(QProcess::Timedout):
    textEdit->append("Error: Timedout");
    break;
  case(QProcess::WriteError):
    textEdit->append("Error: WriteError");
    break;
  case(QProcess::ReadError):
    textEdit->append("Error: ReadError");
    break;
  case(QProcess::UnknownError):
    textEdit->append("Error: UnknownError");
    break;
  default:
    textEdit->append("Error type unknown");
  }
}

void MainWindow::finishedSlot(int exitCode, QProcess::ExitStatus status)
{
  textEdit->setTextColor(Qt::blue);
  textEdit->append("Finished");

  textEdit->append("Exit code: " + QString::number(exitCode));

  switch(status) {
  case(QProcess::NormalExit):
    textEdit->append("Status: NormalExit");
    break;
  case(QProcess::CrashExit):
    textEdit->append("Status: CrashExit");
    break;
  default:
    textEdit->append("Exit status unknown");
  }
}

void MainWindow::stdoutSlot()
{
  QString stdout = elmerGUI->readAllStandardOutput();

  while(stdout.at(stdout.size()-1).unicode() == '\n')
    stdout.chop(1);

  textEdit->setTextColor(Qt::black);
  textEdit->append(stdout);
}

void MainWindow::stderrSlot()
{
  QString stderr = elmerGUI->readAllStandardError();

  while(stderr.at(stderr.size()-1).unicode() == '\n')
    stderr.chop(1);

  textEdit->setTextColor(Qt::red);
  textEdit->append(stderr);
}

void MainWindow::startedSlot()
{
  textEdit->setTextColor(Qt::blue);
  textEdit->append("Started");
}

void MainWindow::stateChangedSlot(QProcess::ProcessState state)
{
  textEdit->setTextColor(Qt::blue);
  textEdit->append("Process state changed");

  switch(state) {
  case(QProcess::NotRunning):
    textEdit->append("State: NotRunning");
    break;
  case(QProcess::Starting):
    textEdit->append("State: Starting");
    break;
  case(QProcess::Running):
    textEdit->append("State: Running");
    break;
  default:
    textEdit->append("Process state unknown");
  }
}

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
 *  ELMER/Mesh3D sifwindow                                                   *
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

#include <QtGui>
#include <iostream>
#include "sifwindow.h"

SifWindow::SifWindow(QWidget *parent)
  : QMainWindow(parent)
{
  setWindowFlags(Qt::Window);

  textEdit = new QTextEdit;
  textEdit->setLineWrapMode(QTextEdit::NoWrap);

  setCentralWidget(textEdit);

  createActions();
  createMenus();
  createToolBars();
  createStatusBar();

  setWindowTitle(tr("Editor"));
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
  return QSize(640, 640);
}

void SifWindow::createActions()
{
  openAct = new QAction(QIcon(":/icons/document-open.png"), tr("&Open..."), this);
  openAct->setShortcut(tr("Ctrl+O"));
  openAct->setStatusTip(tr("Open text file"));
  connect(openAct, SIGNAL(triggered()), this, SLOT(openSlot()));

  saveAct = new QAction(QIcon(":/icons/document-save.png"), tr("&Save..."), this);
  saveAct->setShortcut(tr("Ctrl+S"));
  saveAct->setStatusTip(tr("Save text file"));
  connect(saveAct, SIGNAL(triggered()), this, SLOT(saveSlot()));

  printAct = new QAction(QIcon(":/icons/document-print.png"), tr("&Print..."), this);
  printAct->setShortcut(tr("Ctrl+P"));
  printAct->setStatusTip(tr("Print documtnt"));
  connect(printAct, SIGNAL(triggered()), this, SLOT(printSlot()));

  exitAct = new QAction(QIcon(":/icons/application-exit.png"), tr("&Quit"), this);
  exitAct->setShortcut(tr("Ctrl+Q"));
  exitAct->setStatusTip(tr("Quit editor"));
  connect(exitAct, SIGNAL(triggered()), this, SLOT(exitSlot()));

  clearAct = new QAction(QIcon(":/icons/edit-clear.png"), tr("Clear"), this);
  // clearAct->setShortcut(tr("Ctrl+C"));
  clearAct->setStatusTip(tr("Clear contents"));
  connect(clearAct, SIGNAL(triggered()), this, SLOT(clearSlot()));
}

void SifWindow::createMenus()
{
  fileMenu = menuBar()->addMenu(tr("&File"));
  fileMenu->addAction(openAct);
  fileMenu->addAction(saveAct);
  fileMenu->addAction(printAct);
  fileMenu->addAction(exitAct);

  editMenu = menuBar()->addMenu(tr("&Edit"));
  editMenu->addAction(clearAct);
}

void SifWindow::createToolBars()
{
  fileToolBar = addToolBar(tr("&File"));
  fileToolBar->addAction(openAct);
  fileToolBar->addAction(saveAct);
  fileToolBar->addAction(printAct);
  fileToolBar->addAction(exitAct);

  editToolBar = addToolBar(tr("&Edit"));
  editToolBar->addAction(clearAct);
}

void SifWindow::createStatusBar()
{
  statusBar()->showMessage(tr("Ready"), 2000);  
}

void SifWindow::openSlot()
{
  QString fileName;
  
  fileName = QFileDialog::getOpenFileName(this, tr("Open text file"));

  if(fileName.isEmpty())
    return;

  QFile file;
  file.setFileName(fileName);
  if(!file.open(QIODevice::ReadOnly))
    return;
  
  QTextStream inputStream(&file);
  QString line;

  statusBar()->showMessage(tr("Opening file..."), 2000);

  textEdit->clear();

  do {
    line = inputStream.readLine();
    textEdit->append(line);
  } while(!line.isNull());

  file.close();

  statusBar()->showMessage(tr("Ready"), 2000);
}

void SifWindow::saveSlot()
{
  QString fileName;
  
  fileName = QFileDialog::getSaveFileName(this, tr("Save text file"));

  if(fileName.isEmpty())
    return;

  QFile file;
  file.setFileName(fileName);
  if(!file.open(QIODevice::WriteOnly))
    return;
  
  QTextStream outputStream(&file);

  statusBar()->showMessage(tr("Saving file..."), 2000);

  outputStream << textEdit->toPlainText();

  file.close();

  statusBar()->showMessage(tr("Ready"), 2000);
}

void SifWindow::printSlot()
{
  QTextDocument *document = textEdit->document();
  QPrinter printer;

  QPrintDialog *printDialog = new QPrintDialog(&printer, this);
  if (printDialog->exec() != QDialog::Accepted)
    return;
  
  document->print(&printer);
  
  statusBar()->showMessage(tr("Ready"), 2000);
}

void SifWindow::exitSlot()
{
  this->close();
}

void SifWindow::clearSlot()
{
  textEdit->clear();
}

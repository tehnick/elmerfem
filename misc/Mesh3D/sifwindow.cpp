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

  lineEdit = new QLineEdit;

  createActions();
  createMenus();
  createToolBars();
  createStatusBar();

  firstTime = true;

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
  newAct = new QAction(QIcon(":/icons/document-new.png"), tr("&New"), this);
  newAct->setShortcut(tr("Ctrl+N"));
  newAct->setStatusTip(tr("New text document"));
  connect(newAct, SIGNAL(triggered()), this, SLOT(newSlot()));

  openAct = new QAction(QIcon(":/icons/document-open.png"), tr("&Open..."), this);
  openAct->setShortcut(tr("Ctrl+O"));
  openAct->setStatusTip(tr("Open text file"));
  connect(openAct, SIGNAL(triggered()), this, SLOT(openSlot()));

  saveAct = new QAction(QIcon(":/icons/document-save.png"), tr("&Save as..."), this);
  saveAct->setShortcut(tr("Ctrl+S"));
  saveAct->setStatusTip(tr("Save text file"));
  connect(saveAct, SIGNAL(triggered()), this, SLOT(saveSlot()));

  printAct = new QAction(QIcon(":/icons/document-print.png"), tr("&Print..."), this);
  printAct->setShortcut(tr("Ctrl+P"));
  printAct->setStatusTip(tr("Print document"));
  connect(printAct, SIGNAL(triggered()), this, SLOT(printSlot()));

  exitAct = new QAction(QIcon(":/icons/application-exit.png"), tr("&Quit"), this);
  exitAct->setShortcut(tr("Ctrl+Q"));
  exitAct->setStatusTip(tr("Quit editor"));
  connect(exitAct, SIGNAL(triggered()), this, SLOT(close()));

  cutAct = new QAction(QIcon(":/icons/edit-cut.png"), tr("Cu&t"), this);
  cutAct->setShortcut(tr("Ctrl+X"));
  cutAct->setStatusTip(tr("Cut the current selection to clipboard"));
  connect(cutAct, SIGNAL(triggered()), this->textEdit, SLOT(cut()));

  copyAct = new QAction(QIcon(":/icons/edit-copy.png"), tr("&Copy"), this);
  copyAct->setShortcut(tr("Ctrl+C"));
  copyAct->setStatusTip(tr("Copy the current selection to clipboard"));
  connect(copyAct, SIGNAL(triggered()), this->textEdit, SLOT(copy()));

  pasteAct = new QAction(QIcon(":/icons/edit-paste.png"), tr("&Paste"), this);
  pasteAct->setShortcut(tr("Ctrl+V"));
  pasteAct->setStatusTip(tr("Paste clipboard into the current selection"));
  connect(pasteAct, SIGNAL(triggered()), this->textEdit, SLOT(paste()));

  findAct = new QAction(QIcon(":/icons/edit-find.png"), tr("&Find"), this);
  findAct->setShortcut(tr("Ctrl+F"));
  findAct->setStatusTip(tr("Find specific string"));
  connect(findAct, SIGNAL(triggered()), this, SLOT(findSlot()));

  clearAct = new QAction(QIcon(":/icons/edit-clear.png"), tr("Clear"), this);
  clearAct->setStatusTip(tr("Clear document"));
  connect(clearAct, SIGNAL(triggered()), this, SLOT(clearSlot()));
}

void SifWindow::createMenus()
{
  fileMenu = menuBar()->addMenu(tr("&File"));
  fileMenu->addAction(newAct);
  fileMenu->addAction(openAct);
  fileMenu->addAction(saveAct);
  fileMenu->addSeparator();
  fileMenu->addAction(printAct);
  fileMenu->addSeparator();
  fileMenu->addAction(exitAct);

  editMenu = menuBar()->addMenu(tr("&Edit"));
  editMenu->addAction(cutAct);
  editMenu->addAction(copyAct);
  editMenu->addAction(pasteAct);
  editMenu->addSeparator();
  editMenu->addAction(findAct);
  editMenu->addSeparator();
  editMenu->addAction(clearAct);
}

void SifWindow::createToolBars()
{
  fileToolBar = addToolBar(tr("&File"));
  fileToolBar->addAction(newAct);
  fileToolBar->addAction(openAct);
  fileToolBar->addAction(saveAct);
  fileToolBar->addAction(printAct);
  fileToolBar->addSeparator();
  fileToolBar->addAction(exitAct);

  editToolBar = addToolBar(tr("&Edit"));
  editToolBar->addAction(cutAct);
  editToolBar->addAction(copyAct);
  editToolBar->addAction(pasteAct);
  editToolBar->addSeparator();
  editToolBar->addWidget(lineEdit);
  editToolBar->addAction(findAct);
  editToolBar->addSeparator();
  editToolBar->addAction(clearAct);
}

void SifWindow::createStatusBar()
{
  statusBar()->showMessage(tr("Ready"));
}

void SifWindow::newSlot()
{
  textEdit->clear();

  statusBar()->showMessage(tr("Ready"));
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

  statusBar()->showMessage(tr("Opening file..."));

  textEdit->clear();

  QString line = inputStream.readAll();

  file.close();

  textEdit->append(line);

  statusBar()->showMessage(tr("Ready"));
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

  statusBar()->showMessage(tr("Saving file..."));

  outputStream << textEdit->toPlainText();

  file.close();

  statusBar()->showMessage(tr("Ready"));
}

void SifWindow::printSlot()
{
  QTextDocument *document = textEdit->document();
  QPrinter printer;

  statusBar()->showMessage(tr("Printing..."));

  QPrintDialog *printDialog = new QPrintDialog(&printer, this);
  if (printDialog->exec() != QDialog::Accepted)
    return;
  
  statusBar()->showMessage(tr("Printing..."));

  document->print(&printer);
  
  statusBar()->showMessage(tr("Ready"));
}

void SifWindow::findSlot()
{
  // the following code snippet is from Qt's "textfinder" example:
  QString searchString = lineEdit->text().trimmed();
  QTextDocument *document = textEdit->document();
  
  bool found = false;
  
  if(firstTime == false)
    document->undo();
  
  if(searchString == "") {
    QMessageBox::information(this, tr("Empty search field"),
			     "String to search for is empty");
  } else {

    QTextCursor highlightCursor(document);  
    QTextCursor cursor(document);
    
    cursor.beginEditBlock();
    
    QTextCharFormat plainFormat(highlightCursor.charFormat());
    QTextCharFormat colorFormat = plainFormat;
    colorFormat.setForeground(Qt::red);
    
    while(!highlightCursor.isNull() && !highlightCursor.atEnd()) {
      highlightCursor = document->find(searchString, 
				       highlightCursor,
				       QTextDocument::FindWholeWords);
      
      if(!highlightCursor.isNull()) {
	found = true;
	highlightCursor.movePosition(QTextCursor::WordRight,
				     QTextCursor::KeepAnchor);
	highlightCursor.mergeCharFormat(colorFormat);
      }
    }
    
    cursor.endEditBlock();
    firstTime = false;
    
    if(found == false) {
      QMessageBox::information(this, tr("String not found"),
			       "String to search for was not found");
    }
  }

  statusBar()->showMessage(tr("Ready"));
}

void SifWindow::clearSlot()
{
  textEdit->clear();

  statusBar()->showMessage(tr("Ready"));
}

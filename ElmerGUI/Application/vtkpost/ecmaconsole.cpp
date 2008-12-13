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
 *  ElmerGUI ecmaconsole                                                     *
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

#include "ecmaconsole.h"

#include <QWidget>
#include <QKeyEvent>
#include <QMouseEvent>
#include <QTextCursor>
#include <QTextBlock>

EcmaConsole::EcmaConsole(QWidget* parent)
  : QTextEdit(parent)
{
  prompt = "js>";
  this->clearHistory();
}

EcmaConsole::~EcmaConsole()
{
}

void EcmaConsole::mouseDoubleClickEvent(QMouseEvent* event)
{
  event->ignore();
}

void EcmaConsole::mousePressEvent(QMouseEvent* event)
{
  event->ignore();
}

void EcmaConsole::mouseReleaseEvent(QMouseEvent* event)
{
  event->ignore();
}

void EcmaConsole::keyPressEvent(QKeyEvent* event)
{
  switch(event->key()) {
  case Qt::Key_Return:
    execLine();
    event->ignore();
    break;
    
  case Qt::Key_Up:
    if(historyPtr > 0) {
      historyPtr--;
      scanHistory();
    }    
    event->ignore();
    break;

  case Qt::Key_Down:
    if(historyPtr < history.count()-1) {
      historyPtr++;
      scanHistory();
    }
    event->ignore();
    break;

  case Qt::Key_Left:
  case Qt::Key_Backspace:
  case Qt::Key_Backtab:
    if(this->textCursor().position() <= getPromptPos()) {
      event->ignore();
      break;
    }
    QTextEdit::keyPressEvent(event);
    event->accept();
    break;

  default:
    QTextEdit::keyPressEvent(event);
    event->accept();
    break;
  }
}

int EcmaConsole::getPromptPos()
{
  QTextCursor textCursor(this->textCursor());
  textCursor.movePosition(QTextCursor::End);
  int position = textCursor.block().position() + prompt.length();
  return position;
}

void EcmaConsole::execLine()
{
  QTextCursor textCursor = this->textCursor();
  textCursor.movePosition(QTextCursor::End);
  textCursor.setPosition(getPromptPos());
  textCursor.movePosition(QTextCursor::End, QTextCursor::KeepAnchor);
  QString line = textCursor.selectedText().trimmed();

  if(!line.isEmpty()) {
    emit(cmd(line));
    history << line;
    historyPtr = history.count();
  }

  this->append(prompt);
  textCursor = this->textCursor();
  textCursor.movePosition(QTextCursor::End);
  setTextCursor(textCursor);
}

void EcmaConsole::scanHistory()
{
  QTextCursor textCursor = this->textCursor();
  textCursor.movePosition(QTextCursor::End);
  textCursor.setPosition(getPromptPos(), QTextCursor::KeepAnchor);
  textCursor.insertText(history.value(historyPtr));
  textCursor.movePosition(QTextCursor::End);
  setTextCursor(textCursor);
}

void EcmaConsole::clearHistory()
{
  this->clear();
  history.clear();
  historyPtr = 0;
  this->append(prompt);
}

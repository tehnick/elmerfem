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
  this->append(prompt);
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
  case Qt::Key_Down:
    event->ignore();
    break;

  case Qt::Key_Left:
  case Qt::Key_Backspace:
  case Qt::Key_Backtab:
    if(this->textCursor().position() <= getPosition()) {
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

int EcmaConsole::getPosition()
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
  textCursor.setPosition(getPosition());
  textCursor.movePosition(QTextCursor::End, QTextCursor::KeepAnchor);
  QString line = textCursor.selectedText().trimmed();

  if(!line.isEmpty()) emit(cmd(line));

  this->append(prompt);
  textCursor = this->textCursor();
  textCursor.movePosition(QTextCursor::End);
  setTextCursor(textCursor);
}

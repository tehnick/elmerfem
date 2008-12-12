#ifndef ECMACONSOLE_H
#define ECMACONSOLE_H

#include <QTextEdit>
#include <QString>

class QWidget;
class QKeyEvent;
class QMouseEvent;

class EcmaConsole : public QTextEdit
{
  Q_OBJECT

public:
  EcmaConsole(QWidget* parent = 0);
  ~EcmaConsole();

public slots:
  void keyPressEvent(QKeyEvent*);
  void mousePressEvent(QMouseEvent*);
  void mouseReleaseEvent(QMouseEvent*);
  void mouseDoubleClickEvent(QMouseEvent*);

signals:
  void cmd(QString);

private:
  QString prompt;
  int getPosition();
  void execLine();
};

#endif

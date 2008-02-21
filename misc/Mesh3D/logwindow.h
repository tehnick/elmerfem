#ifndef LOGWINDOW_H
#define LOGWINDOW_H

#include <QWidget>

class QPushButton;
class QTextEdit;

class LogWindow : public QWidget
{
  Q_OBJECT

public:
  LogWindow(QWidget *parent = 0);
  ~LogWindow();

  QSize minimumSizeHint() const;
  QSize sizeHint() const;

  QTextEdit *textEdit;


private:
  QPushButton *closeButton;

};

#endif

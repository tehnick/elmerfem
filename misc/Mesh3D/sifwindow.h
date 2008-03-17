#ifndef SIFWINDOW_H
#define SIFWINDOW_H

#include <QWidget>

class QTextEdit;
class QPushButton;
class QMenu;

class SifWindow : public QWidget
{
  Q_OBJECT

public:
  SifWindow(QWidget *parent = 0);
  ~SifWindow();

  QSize minimumSizeHint() const;
  QSize sizeHint() const;

  QTextEdit *textEdit;

private slots:
  void clearSif();

private:
  QPushButton *closeButton;
  QPushButton *clearButton;

};

#endif

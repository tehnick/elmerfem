#ifndef SIFWINDOW_H
#define SIFWINDOW_H

#include <QMainWindow>

class QTextEdit;
class QLineEdit;

class SifWindow : public QMainWindow
{
  Q_OBJECT

public:
  SifWindow(QWidget *parent = 0);
  ~SifWindow();

  QSize minimumSizeHint() const;
  QSize sizeHint() const;

  QTextEdit *textEdit;
  bool found;

private slots:
  void newSlot();
  void openSlot();
  void saveSlot();
  void printSlot();
  void findSlot();

private:
  QLineEdit *lineEdit;

  QAction *newAct;
  QAction *openAct;
  QAction *saveAct;
  QAction *printAct;
  QAction *exitAct;
  QAction *cutAct;
  QAction *copyAct;
  QAction *pasteAct;
  QAction *findAct;

  QMenu *fileMenu;
  QMenu *editMenu;

  QToolBar *fileToolBar;
  QToolBar *editToolBar;

  void createActions();
  void createMenus();
  void createToolBars();
  void createStatusBar();
};

#endif

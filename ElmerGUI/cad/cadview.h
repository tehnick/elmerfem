#ifndef CADVIEW_H
#define CADVIEW_H

#include <QMainWindow>
#include "qocc.h"
#include "qoccinternal.h"
#include "qoccviewercontext.h"
#include "qoccviewwidget.h"

class CadView : public QMainWindow
{
  Q_OBJECT

public:
  CadView(QWidget *parent = 0);
  ~CadView();

  QSize minimumSizeHint() const;
  QSize sizeHint() const;

  Handle_TopTools_HSequenceOfShape shapes;
  QoccViewWidget *myOCC;
  QoccViewerContext *myVC;

  bool convertToSTL(QString, QString);
  void drawModel();

private slots:
  void fitToWindowSlot();

private:
  QMenu *fileMenu;
  QMenu *viewMenu;

  QAction *exitAct;
  QAction *fitToWindowAct;

  void createActions();
  void createMenus();
};

#endif // CADVIEW_H

#ifndef CADVIEW_H
#define CADVIEW_H

#include <QMainWindow>
#ifdef OCC62
#include "qocc.h"
#include "qoccinternal.h"
#include "qoccviewercontext.h"
#include "qoccviewwidget.h"
#endif

class CadView : public QMainWindow
{
  Q_OBJECT

public:
  CadView(QWidget *parent = 0);
  ~CadView();

  QSize minimumSizeHint() const;
  QSize sizeHint() const;

#ifdef OCC62
  QoccViewWidget *myOCC;
  QoccViewerContext *myVC;
#endif

private slots:

private:

};

#endif // CADVIEW_H

#ifndef CONVERGENCEVIEW_H
#define CONVERGENCEVIEW_H

#include <QMainWindow>

class QGraphicsView;

class ConvergenceView : public QMainWindow
{
  Q_OBJECT

public:
  ConvergenceView(QWidget *parent = 0);
  ~ConvergenceView();

  QSize minimumSizeHint() const;
  QSize sizeHint() const;

private slots:

private:
  QGraphicsView *view;

};

#endif // CONVERGENCEVIEW_H

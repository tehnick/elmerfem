#ifndef CONVERGENCEVIEW_H
#define CONVERGENCEVIEW_H

#include <QMainWindow>

class QGraphicsScene;
class QGraphicsView;

class ConvergenceView : public QMainWindow
{
  Q_OBJECT

public:
  ConvergenceView(QWidget *parent = 0);
  ~ConvergenceView();

  QSize minimumSizeHint() const;
  QSize sizeHint() const;

  QGraphicsScene *scene;
  QGraphicsView *view;

private slots:

private:

};

#endif // CONVERGENCEVIEW_H

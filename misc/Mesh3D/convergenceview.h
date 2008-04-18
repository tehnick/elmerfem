#ifndef CONVERGENCEVIEW_H
#define CONVERGENCEVIEW_H

#include <QMainWindow>
#include <QHash>
#include "maxlimits.h"

#include <qwt_plot.h>
#include <qwt_plot_marker.h>
#include <qwt_plot_curve.h>
#include <qwt_plot_grid.h>
#include <qwt_legend.h>
#include <qwt_data.h>
#include <qwt_text.h>
#include <qwt_scale_engine.h>

class CurveData
{
public:
  CurveData();
  
  void append(double*, double*, int);
  
  int count() const;
  int size() const;
  const double *x() const;
  const double *y() const;
  
private:
  int d_count;
  QwtArray<double> d_x;
  QwtArray<double> d_y;
};

class Curve
{
public:
  CurveData *d_data;
  QwtPlotCurve *d_curve;
};

class ConvergenceView : public QMainWindow
{
  Q_OBJECT

public:
  ConvergenceView(QWidget *parent = 0);
  ~ConvergenceView();

  QSize minimumSizeHint() const;
  QSize sizeHint() const;

  void appendData(double, QString);
  void appendData(double*, int, QString);
  void removeData();

  QString title;

private:
  QwtPlot *plot;

  QHash<QString, Curve*> curveList;
  QPen pen[MAX_EQUATIONS];
};

#endif // CONVERGENCEVIEW_H

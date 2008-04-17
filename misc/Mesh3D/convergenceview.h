#ifndef CONVERGENCEVIEW_H
#define CONVERGENCEVIEW_H

#include <QHash>
#include <qwt_plot.h>
#include <qwt_plot_marker.h>
#include <qwt_plot_curve.h>
#include <qwt_plot_grid.h>
#include <qwt_legend.h>
#include <qwt_data.h>
#include <qwt_text.h>
#include <qwt_scale_engine.h>
#include "maxlimits.h"

class CurveData
{
public:
  CurveData();
  
  void append(double *x, double *y, int count);
  
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

class ConvergenceView : public QwtPlot
{
public:
  ConvergenceView();
  ~ConvergenceView();

  void appendData(double x, double y, QString name);
  void appendData(double *x, double *y, int size, QString name);
  void removeData();

private:
  QHash<QString, Curve*> curveList;
  CurveData *d_data;
  QwtPlotCurve *d_curve;
  QPen pen[MAX_EQUATIONS];
};

#endif // CONVERGENCEVIEW_H

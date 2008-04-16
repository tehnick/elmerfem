#ifndef CONVERGENCEVIEW_H
#define CONVERGENCEVIEW_H

#include <qwt_plot.h>
#include <qwt_plot_marker.h>
#include <qwt_plot_curve.h>
#include <qwt_legend.h>
#include <qwt_data.h>
#include <qwt_text.h>
#include <qwt_scale_engine.h>

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


class ConvergenceView : public QwtPlot
{
public:
  ConvergenceView();
  ~ConvergenceView();

  void appendData(double x, double y);
  void appendData(double *x, double *y, int size);
  void removeData();

private:
  CurveData *d_data;
  QwtPlotCurve *d_curve;
};

#endif // CONVERGENCEVIEW_H

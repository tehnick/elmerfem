#ifndef CONVERGENCEVIEW_H
#define CONVERGENCEVIEW_H

#include <qwt_plot.h>
#include <qwt_plot_marker.h>
#include <qwt_plot_curve.h>
#include <qwt_legend.h>
#include <qwt_data.h>
#include <qwt_text.h>
#include <math.h>

class ConvergenceView : public QwtPlot
{
public:
  ConvergenceView();
  ~ConvergenceView();
};

#endif // CONVERGENCEVIEW_H

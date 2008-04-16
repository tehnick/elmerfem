/*****************************************************************************
 *                                                                           *
 *  Elmer, A Finite Element Software for Multiphysical Problems              *
 *                                                                           *
 *  Copyright 1st April 1995 - , CSC - Scientific Computing Ltd., Finland    *
 *                                                                           *
 *  This program is free software; you can redistribute it and/or            *
 *  modify it under the terms of the GNU General Public License              *
 *  as published by the Free Software Foundation; either version 2           *
 *  of the License, or (at your option) any later version.                   *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *
 *  GNU General Public License for more details.                             *
 *                                                                           *
 *  You should have received a copy of the GNU General Public License        *
 *  along with this program (in file fem/GPL-2); if not, write to the        *
 *  Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,         *
 *  Boston, MA 02110-1301, USA.                                              *
 *                                                                           *
 *****************************************************************************/

/*****************************************************************************
 *                                                                           *
 *  ELMER/Mesh3D convergenceview                                             *
 *                                                                           *
 *****************************************************************************
 *                                                                           *
 *  Authors: Mikko Lyly, Juha Ruokolainen and Peter Råback                   *
 *  Email:   Juha.Ruokolainen@csc.fi                                         *
 *  Web:     http://www.csc.fi/elmer                                         *
 *  Address: CSC - Scientific Computing Ltd.                                 *
 *           Keilaranta 14                                                   *
 *           02101 Espoo, Finland                                            *
 *                                                                           *
 *  Original Date: 15 Mar 2008                                               *
 *                                                                           *
 *****************************************************************************/

#include <iostream>
#include "convergenceview.h"

using namespace std;

CurveData::CurveData():
  d_count(0)
{
}

void CurveData::append(double *x, double *y, int count)
{
  int newSize = ((d_count + count) / 1000 + 1) * 1000;
  if(newSize > size()) {
    d_x.resize(newSize);
    d_y.resize(newSize);
  }
  
  for(register int i = 0; i < count; i++) {
    d_x[d_count + i] = x[i];
    d_y[d_count + i] = y[i];
  }
  
  d_count += count;
}

int CurveData::count() const
{
  return d_count;
}

int CurveData::size() const
{
  return d_x.size();
}

const double *CurveData::x() const
{
  return d_x.data();
}

const double *CurveData::y() const
{
  return d_y.data();
}


ConvergenceView::ConvergenceView() :
  d_data(NULL),
  d_curve(NULL)
{
  setAutoReplot(true);
  setCanvasBackground(QColor(Qt::white));
  setTitle("Nonlinear system convergence");

  // legend
  QwtLegend *legend = new QwtLegend;
  legend->setFrameStyle(QFrame::Box|QFrame::Sunken);
  insertLegend(legend, QwtPlot::RightLegend);

  // grid
  QwtPlotGrid *grid = new QwtPlotGrid;
  grid->enableXMin(true);
  grid->setMajPen(QPen(Qt::black, 0, Qt::DotLine));
  grid->setMinPen(QPen(Qt::gray, 0 , Qt::DotLine));
  // grid->attach(this);
  
  // axes
  setAxisTitle(xBottom, "Iteration step");
  setAxisTitle(yLeft, "Relative change");

  setAxisMaxMajor(QwtPlot::xBottom, 20);
  setAxisMaxMinor(QwtPlot::xBottom, 1);
  setAxisMaxMajor(QwtPlot::yLeft, 10);
  setAxisMaxMinor(QwtPlot::yLeft, 10);

  // scale engine
  QwtLog10ScaleEngine *scaleEngine = new QwtLog10ScaleEngine;
  setAxisScaleEngine(QwtPlot::yLeft, scaleEngine);

  this->resize(600, 400);
  this->setWindowTitle("Convergence monitor");
}

ConvergenceView::~ConvergenceView()
{
  delete d_data;
}


void ConvergenceView::appendData(double x, double y)
{
  appendData(&x, &y, 1);
}

void ConvergenceView::appendData(double *x, double *y, int size)
{
  if(d_data == NULL)
    d_data = new CurveData;
  
  if(d_curve == NULL) {
    d_curve = new QwtPlotCurve(name);
    d_curve->setRenderHint(QwtPlotItem::RenderAntialiased);
    d_curve->setPen(QPen(Qt::red));
    d_curve->attach(this);
  }
  
  d_data->append(x, y, size);
  d_curve->setRawData(d_data->x(), d_data->y(), d_data->count());
}

void ConvergenceView::removeData()
{
  delete d_curve;
  d_curve = NULL;
  
  delete d_data;
  d_data = NULL;
  
  replot();
}

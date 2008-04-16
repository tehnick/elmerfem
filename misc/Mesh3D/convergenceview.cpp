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

ConvergenceView::ConvergenceView()
{
  setTitle("Convergence");
  insertLegend(new QwtLegend(), QwtPlot::RightLegend);

  setAxisTitle(xBottom, "Iteration");
  setAxisTitle(yLeft, "Residual");

  residual = new QwtPlotCurve("Nonlinear system");
  residual->setRenderHint(QwtPlotItem::RenderAntialiased);
  residual->setPen(QPen(Qt::red));
  residual->attach(this);

  this->resize(600, 400);

  // set data and plot (test):
#if 0
  int n = 2;
  double x[2], y[2];
  x[0] = 0.0;
  x[1] = 10.0;
  y[0] = 2.0;
  y[1] = 1.5;
  residual->setData(x, y, n);
#endif
}

ConvergenceView::~ConvergenceView()
{
}

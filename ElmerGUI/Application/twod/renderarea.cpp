/*****************************************************************************
 *                                                                           *
 *  Elmer, A Finite Element Software for Multiphysical Problems              *
 *                                                                           *
 *  Copyright 1st April 1995 - , CSC - IT Center for Science Ltd., Finland    *
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
 *  ElmerGUI RenderArea                                                      *
 *                                                                           *
 *****************************************************************************
 *                                                                           *
 *  Authors: Mikko Lyly, Juha Ruokolainen and Peter Råback                   *
 *  Email:   Juha.Ruokolainen@csc.fi                                         *
 *  Web:     http://www.csc.fi/elmer                                         *
 *  Address: CSC - IT Center for Science Ltd.                                 *
 *           Keilaranta 14                                                   *
 *           02101 Espoo, Finland                                            *
 *                                                                           *
 *  Original Date: 15 Mar 2008                                               *
 *                                                                           *
 *****************************************************************************/
#include <QPainter>
#include <QMouseEvent>
#include <QFile>
#include <QTextStream>
#include <QCoreApplication>
#include <iostream>
#include <math.h>
#include "renderarea.h"
#include "curveeditor.h"

using namespace std;

RenderArea::RenderArea(QWidget *parent)
  : QWidget(parent)
{
  pointRadius = 3;
  setAutoFillBackground(true);
  setPalette(QPalette(Qt::white));

  QStringList args = QCoreApplication::arguments();

  if(args.count() > 1) {
    readSlot(args.at(1));
    fitSlot();
  }
}

RenderArea::~RenderArea()
{
}

void RenderArea::paintEvent(QPaintEvent * /* event */)
{
  QPainter painter(this);
  painter.setRenderHint(QPainter::Antialiasing);
  this->viewport = painter.viewport();

  QPen pointPen;
  pointPen.setWidth(1);
  pointPen.setColor(Qt::black);
  pointPen.setStyle(Qt::SolidLine);

  QPen splinePen;
  splinePen.setWidth(1);
  splinePen.setColor(Qt::blue);
  splinePen.setStyle(Qt::SolidLine);

  QPen tangentPen;
  tangentPen.setWidth(1);
  tangentPen.setColor(Qt::red);
  tangentPen.setStyle(Qt::SolidLine);

  QPen bodyTextPen;
  bodyTextPen.setWidth(1);
  bodyTextPen.setColor(Qt::green);
  bodyTextPen.setStyle(Qt::SolidLine);
  
  // Draw splines:
  //---------------
  QPainterPath path;

  int j, n = 20;
  double u;
  QPointF p, q, t1, t2;

  QPointF offset(-pointRadius, pointRadius/2);
  
  for(int i = 0; i < splines.keys().size(); i++) {
    int idx = splines.keys().at(i);
    Spline s = splines.value(idx);
    
    QPointF p0 = points.value(s.p[0]);
    QPointF p1 = points.value(s.p[1]);
    QPointF p2 = points.value(s.p[2]);
    
    QPointF q0 = mapToViewport(p0);
    QPointF q1 = mapToViewport(p1);
    QPointF q2 = mapToViewport(p2);
    
    switch(s.np) {
    case 2:
      painter.setPen(splinePen);
      path.moveTo(q0);

      // Draw linear segment:
      //---------------------
      if(drawSplines)
	path.lineTo(q1);
      
      // Draw spline number:
      //---------------------
      painter.setPen(pointPen);

      q = (q1 + q0)/2.0;

      if(drawSplineNumbers)
	painter.drawText(q + offset, QString::number(idx));
      
      // Draw material numbers:
      //------------------------
      t1 = q1 - q0;
      t2.setX(-t1.y());
      t2.setY(t1.x());
      t2 /= sqrt(t2.x()*t2.x() + t2.y()*t2.y());
      t2 *= 10;
      
      painter.setPen(bodyTextPen);

      if(drawMaterialNumbers) {
	painter.drawText(q + t2 + offset, QString::number(s.in));
	painter.drawText(q - t2 + offset, QString::number(s.out));
      }
      
      break;
      
    case 3:
      path.moveTo(q0);
      
      // Draw quadratic nurbs:
      //-----------------------
      for(j = 0; j <= n; j++) {
	u = double(j) / double(n);
	p = quadNurbs(u, q0, q1, q2);

	if(drawSplines)
	  path.lineTo(p);
      }
      
      // Draw spline number:
      //---------------------
      painter.setPen(pointPen);

      q = quadNurbs(0.5, q0, q1, q2);

      if(drawSplineNumbers)
	painter.drawText(q + offset, QString::number(idx));
      
      // Draw material numbers:
      //------------------------
      t1 = q2 - q0;
      t2.setX(-t1.y());
      t2.setY(t1.x());
      t2 /= sqrt(t2.x()*t2.x() + t2.y()*t2.y());
      t2 *= 10;
      
      painter.setPen(bodyTextPen);

      if(drawMaterialNumbers) {
	painter.drawText(q + t2 + offset, QString::number(s.in));
	painter.drawText(q - t2 + offset, QString::number(s.out));
      }
      
      // Draw control tangents:
      //------------------------
      painter.setPen(tangentPen);

      if(drawTangents) {
	painter.drawLine(q0, q1);
	painter.drawLine(q1, q2);
      }
      
      break;
      
    default:
      break;
    }
  }
  
  // Draw spline path:
  //-------------------
  painter.setPen(splinePen);  
  painter.drawPath(path);

  // Draw points:
  //--------------
  QPointF offset2(pointRadius*1.5, pointRadius);

  painter.setPen(pointPen);
  for(int i = 0; i < points.keys().size(); i++) {
    int idx = points.keys().at(i);
    QPointF p = points.value(idx);
    QPointF q = mapToViewport(p);
    
    if(drawPoints)
      painter.drawEllipse(q, pointRadius, pointRadius);
    
    if(drawPointNumbers)
      painter.drawText(q + offset2, QString::number(idx));
  }
}

void RenderArea::wheelEvent(QWheelEvent *event)
{
  double s = exp((double)(event->delta())*0.001);
  double width = renderport.width();
  double height = renderport.height();
  renderport.setWidth(s*width);
  renderport.setHeight(s*height);
  renderport.translate(QPointF(0.5*(1-s)*width, 0.5*(1-s)*height));
  lastPos = event->pos();
  update();
}

void RenderArea::mousePressEvent(QMouseEvent *event)
{
  selectedPoint = -1;

  for(int i = 0; i < points.keys().size(); i++) {
    int idx = points.keys().at(i);
    QPointF p = points.value(idx);
    QPointF q = mapToViewport(p);

    double d = (event->x() - q.x()) * (event->x() - q.x())
             + (event->y() - q.y()) * (event->y() - q.y());
    
    if(d <= (pointRadius * pointRadius)) {
      QString message = "Point " + QString::number(idx);
      emit(statusMessage(message));
      selectedPoint = idx;
      break;
    }
  }

  lastPos = event->pos();
}

void RenderArea::mouseReleaseEvent(QMouseEvent *event)
{
  selectedPoint = -1;

  lastPos = event->pos();

  emit(statusMessage("Ready"));
}

void RenderArea::mouseMoveEvent(QMouseEvent *event)
{
  QPointF p, q;
  QString message;
  double a, b, scale, dx, dy;
  
  switch(event->buttons()) {

  case Qt::LeftButton:
    if(selectedPoint < 0)
      return;
    
    // Move point:
    //------------
    p.setX(event->x());
    p.setY(event->y());
    q = mapToRenderport(p);
    points.insert(selectedPoint, q);
    
    update();
    
    message = QString::number(q.x()) + " "  + QString::number(q.y());
    
    emit(statusMessage(message));

    break;

  case Qt::RightButton:
    a = renderport.height();
    b = viewport.height();
    scale = a/b;
    dx = scale * (double(event->pos().x()) - double(lastPos.x()));
    dy = scale * (double(event->pos().y()) - double(lastPos.y()));
    p.setX(-dx);
    p.setY(dy);

    // Pan:
    //------
    renderport.translate(p);
    update();

    break;

  default:
    break;
  }

  lastPos = event->pos();
}

QPointF RenderArea::mapToViewport(QPointF point) const
{
  QPointF mapped;

  double h = renderport.height();
  double x = renderport.x();
  double y = renderport.y();
  double xi = (point.x() - x) / h;
  double eta = (point.y() - y) / h;

  double h0 = viewport.height();
  double x0 = viewport.x();
  double y0 = viewport.y();

  mapped.setX(x0 + xi * h0);
  mapped.setY(y0 + h0 - eta * h0);
  
  return mapped;
}

QPointF RenderArea::mapToRenderport(QPointF point) const
{
  QPointF mapped;

  double h = viewport.height();
  double x = viewport.x();
  double y = viewport.y();
  double xi = (point.x() - x) / h;
  double eta = 1.0 - (point.y() - y) / h;

  double h0 = renderport.height();
  double x0 = renderport.x();
  double y0 = renderport.y();

  mapped.setX(x0 + xi * h0);
  mapped.setY(y0 + eta * h0);

  return mapped;
}

void RenderArea::fitSlot()
{
  double xmin = 9e99;
  double xmax = -9e99;
  double ymin = 9e99;
  double ymax = -9e99;

  for(int i = 0; i < points.keys().size(); i++) {
    int idx = points.keys().at(i);
    QPointF p = points.value(idx);

    xmin = qMin(xmin, p.x());
    xmax = qMax(xmax, p.x());
    ymin = qMin(ymin, p.y());
    ymax = qMax(ymax, p.y());
  }
  
  double width = xmax - xmin;
  double height = ymax - ymin;
  
  double oh = qMax(width, height) * 0.1;

  renderport.setRect(xmin-oh, ymin-oh, width+2*oh, height+2*oh);

  update();

  emit(statusMessage("Ready"));
}

void RenderArea::readSlot(QString fileName)
{
  QFile file(fileName);

  if (!file.open(QIODevice::ReadOnly | QIODevice::Text))
    return;

  points.clear();
  splines.clear();
  curveEditor->clearAll();

  int mode = 0;
  int index, i;
  double x, y;
  QPointF p;
  Spline s;
  int countSplines = 0;

  // Parse input file:
  //-------------------
  bool correctVersion = false;

  while(!file.atEnd()) {
    QByteArray line = file.readLine();

    if(line.trimmed().isEmpty())
      continue;
    
    if(line.trimmed().left(1) == "#")
      continue;

    if(line.trimmed() == "splinecurves2dv2") {
      correctVersion = true;
      continue;
    }

    if(line.trimmed() == "points") {
      mode = 1;
      continue;
    }

    if(line.trimmed() == "segments") {
      mode = 2;
      continue;
    }

    if(line.trimmed() == "materials") {
      mode = 3;
      continue;
    }

    QTextStream stream(&line);

    switch(mode) {
    case(1):
      stream >> index >> x >> y;
      p.setX(x); p.setY(y);
      points.insert(index, p);
      curveEditor->addPoint(index, x, y);
      break;

    case(2):
      stream >> s.out >> s.in >> s.np;
      for(i = 0; i < s.np; i++)
	stream >> s.p[i];
      splines.insert(++countSplines, s);
      curveEditor->addCurve(s.in, s.out, s.np, s.p);
      break;

    case(3):
      break;

    default:
      break;
    }
  }

  if(!correctVersion) {
    cout << "Wrong spline geometry format" << endl;
    cout << "splinecurves2dv2 is required" << endl;
    cout << "Aborting" << endl;
    points.clear();
    splines.clear();
    curveEditor->clearAll();
    return;
  }

  // Enumerate bodies:
  //-------------------
  bodies.clear();

  for(int i = 0; i < splines.keys().size(); i++) {
    int idx = splines.keys().at(i);
    Spline s = splines.value(idx);

    if(!bodies.contains(s.in))
      bodies.push_back(s.in);

    if(!bodies.contains(s.out))
      bodies.push_back(s.out);
  }

  cout << "Points: " << points.count() << endl;
  cout << "Splines: " << splines.count() << endl;
  cout << "Bodies: " << bodies.count() - 1 << endl;

  emit(statusMessage("Ready"));
}

QPointF RenderArea::quadNurbs(double u, QPointF P0, QPointF P1, QPointF P2) const
{
  QPointF result;

  double l0 = 1.0 - u;
  double l1 = u;

  double q0 = l0 * l0;
  double q1 = 2.0 * l0 * l1;
  double q2 = l1 * l1;
  
  double w0 = 1.0;
  double w1 = 1.0 / sqrt(2.0);
  double w2 = 1.0;

  result = w0*q0*P0 + w1*q1*P1 + w2*q2*P2;

  result /= w0*q0 + w1*q1 + w2*q2;

  return result;
}

void RenderArea::drawPointsSlot(bool state)
{
  drawPoints = state;
  update();
}

void RenderArea::drawSplinesSlot(bool state)
{
  drawSplines = state;
  update();
}

void RenderArea::drawTangentsSlot(bool state)
{
  drawTangents = state;
  update();
}

void RenderArea::drawPointNumbersSlot(bool state)
{
  drawPointNumbers = state;
  update();
}

void RenderArea::drawSplineNumbersSlot(bool state)
{
  drawSplineNumbers = state;
  update();
}

void RenderArea::drawMaterialNumbersSlot(bool state)
{
  drawMaterialNumbers = state;
  update();
}

void RenderArea::setCurveEditor(CurveEditor *curveEditor)
{
  this->curveEditor = curveEditor;
}

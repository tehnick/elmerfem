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
 *  ElmerGUI CurveEditor                                                     *
 *                                                                           *
 *****************************************************************************
 *                                                                           *
 *  Authors: Mikko Lyly, Juha Ruokolainen and Peter Råback                   *
 *  Email:   Juha.Ruokolainen@csc.fi                                         *
 *  Web:     http://www.csc.fi/elmer                                         *
 *  Address: CSC - IT Center for Science Ltd.                                *
 *           Keilaranta 14                                                   *
 *           02101 Espoo, Finland                                            *
 *                                                                           *
 *  Original Date: 15 Mar 2008                                               *
 *                                                                           *
 *****************************************************************************/
#include <QTableWidget>
#include <iostream>
#include "curveeditor.h"

using namespace std;

CurveEditor::CurveEditor(QWidget *parent)
  : QTabWidget(parent)
{
  pTable = new QTableWidget(0, 3, this);
  cTable = new QTableWidget(0, 6, this);

  addTab(pTable, tr("Points"));
  addTab(cTable, tr("Curves"));

  clearAll();
}

CurveEditor::~CurveEditor()
{
}

void CurveEditor::addPoint(int idx, double x, double y)
{
  int i = pTable->rowCount();
  
  pTable->insertRow(i);
  QTableWidgetItem *item;

  item = new QTableWidgetItem;
  item->setText(QString::number(idx));
  pTable->setItem(i, 0, item);

  item = new QTableWidgetItem;
  item->setText(QString::number(x));
  pTable->setItem(i, 1, item);

  item = new QTableWidgetItem;
  item->setText(QString::number(y));
  pTable->setItem(i, 2, item);
}

void CurveEditor::addCurve(int in, int out, int pts, int *p)
{
  int i = cTable->rowCount();
  
  cTable->insertRow(i);
  QTableWidgetItem *item;

  item = new QTableWidgetItem;
  item->setText(QString::number(in));
  cTable->setItem(i, 0, item);

  item = new QTableWidgetItem;
  item->setText(QString::number(out));
  cTable->setItem(i, 1, item);

  item = new QTableWidgetItem;
  item->setText(QString::number(pts));
  cTable->setItem(i, 2, item);

  item = new QTableWidgetItem;
  item->setText(QString::number(p[0]));
  cTable->setItem(i, 3, item);

  item = new QTableWidgetItem;
  item->setText(QString::number(p[1]));
  cTable->setItem(i, 4, item);

  item = new QTableWidgetItem;
  if(pts == 3) {
    item->setText(QString::number(p[2]));
  } else {
    item->setText("-");
  }
  cTable->setItem(i, 5, item);
}

void CurveEditor::clearAll()
{
  pTable->clear();
  pTable->setRowCount(0);

  cTable->clear();
  cTable->setRowCount(0);

  QStringList pHeaders;
  pHeaders << "idx" << "x" << "y";
  pTable->setHorizontalHeaderLabels(pHeaders);

  pTable->setColumnWidth(0, 40);
  pTable->setColumnWidth(1, 80);
  pTable->setColumnWidth(2, 80);

  QStringList cHeaders;
  cHeaders << "in" << "out" << "pts" << "p1" << "p2" << "p3";
  cTable->setHorizontalHeaderLabels(cHeaders);
  cTable->setColumnWidth(0, 40);
  cTable->setColumnWidth(1, 40);
  cTable->setColumnWidth(2, 40);
  cTable->setColumnWidth(3, 40);
  cTable->setColumnWidth(4, 40);
  cTable->setColumnWidth(5, 40);
}

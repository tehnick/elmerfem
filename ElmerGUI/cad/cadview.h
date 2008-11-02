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
 *  ElmerGUI cadview                                                         *
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

#ifndef CADVIEW_H
#define CADVIEW_H

#include <QMainWindow>
#include "qocc.h"
#include "qoccinternal.h"
#include "qoccviewercontext.h"
#include "qoccviewwidget.h"

class CadView : public QMainWindow
{
  Q_OBJECT

public:
  CadView(QWidget *parent = 0);
  ~CadView();

  QSize minimumSizeHint() const;
  QSize sizeHint() const;

  Handle_TopTools_HSequenceOfShape shapes;
  QoccViewWidget *qoccViewWidget;
  QoccViewerContext *qoccViewerContext;

  bool convertToSTL(QString, QString);
  void drawModel();
  void fitToWindow();

private slots:
  void fitToWindowSlot();
  void helpSlot(); 

private:
  QMenu *fileMenu;
  QMenu *viewMenu;
  QMenu *helpMenu;

  QAction *exitAct;
  QAction *fitToWindowAct;
  QAction *helpAct;

  void createActions();
  void createMenus();
};

#endif // CADVIEW_H

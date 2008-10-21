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
 *  ElmerGUI surface                                                         *
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

#ifndef MATC_H
#define MATC_H

#include <QWidget>
#include "ui_matc.h"

#include "mc.h"

extern "C" VARIABLE *com_curl(VARIABLE *);
extern "C" VARIABLE *com_div(VARIABLE *);
extern "C" VARIABLE *com_grad(VARIABLE *);
extern "C" VARIABLE *var_new(char *,int,int,int);
extern "C" VARIABLE *var_check(char *);
extern "C" VARIABLE *var_temp_new(int,int,int);
extern "C" void var_delete(char *);
extern "C" char *mtc_domath(const char *);
extern "C" void mtc_init(FILE *,FILE *,FILE *);
extern "C" void com_init(char *,int,int,VARIABLE *(*)(VARIABLE *),int,int,char*);

class VtkPost;

class Matc : public QDialog
{
  Q_OBJECT

public:
  Matc(QWidget *parent = 0);
  ~Matc();

  Ui::mcDialog ui;

  void grad(VtkPost*, double*, double*);
  void div(VtkPost*, double*, double*);
  void curl(VtkPost*, double*, double*);
  void domatc(VtkPost*);

private slots:
  void okButtonClicked();

private:
};

#endif // MATC_H

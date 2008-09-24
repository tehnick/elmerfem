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
 *  ElmerGUI projectio                                                       *
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

#include <QtGui>
#include <iostream>
#include "projectio.h"

using namespace std;

ProjectIO::ProjectIO(QWidget *parent)
  : QDialog(parent)
{
  parentWidget = parent;
}

ProjectIO::~ProjectIO()
{
}

void ProjectIO::appendToProject(QDomDocument *projectDoc, QDomElement *item)
{
  // Radio buttons:
  //----------------
  QList<QRadioButton *> allRadioButtons = parentWidget->findChildren<QRadioButton *>(); 
  
  for(int i = 0; i < allRadioButtons.size(); i++) {
    QRadioButton *rb = allRadioButtons.at(i);
    QString rbObjectName = rb->objectName();
    QString rbValue = QString::number(rb->isChecked());

    QDomElement widget = projectDoc->createElement("widget");
    widget.setAttribute("type", "RadioButton");
    item->appendChild(widget);
    
    QDomElement objectName = projectDoc->createElement("objectName");
    QDomText objectNameValue = projectDoc->createTextNode(rbObjectName);
    objectName.appendChild(objectNameValue);
    widget.appendChild(objectName);

    QDomElement isChecked = projectDoc->createElement("isChecked");
    QDomText isCheckedValue = projectDoc->createTextNode(rbValue);
    isChecked.appendChild(isCheckedValue);
    widget.appendChild(isChecked);
  }

  // Check boxes:
  //--------------
  QList<QCheckBox *> allCheckBoxes = parentWidget->findChildren<QCheckBox *>(); 
  
  for(int i = 0; i < allCheckBoxes.size(); i++) {
    QCheckBox *cb = allCheckBoxes.at(i);
    QString cbObjectName = cb->objectName();
    QString cbValue = QString::number(cb->isChecked());

    QDomElement widget = projectDoc->createElement("widget");
    widget.setAttribute("type", "CheckBox");
    item->appendChild(widget);
    
    QDomElement objectName = projectDoc->createElement("objectName");
    QDomText objectNameValue = projectDoc->createTextNode(cbObjectName);
    objectName.appendChild(objectNameValue);
    widget.appendChild(objectName);

    QDomElement isChecked = projectDoc->createElement("isChecked");
    QDomText isCheckedValue = projectDoc->createTextNode(cbValue);
    isChecked.appendChild(isCheckedValue);
    widget.appendChild(isChecked);
  }

  // Line edits:
  //-------------
  QList<QLineEdit *> allLineEdits = parentWidget->findChildren<QLineEdit *>(); 
  
  for(int i = 0; i < allLineEdits.size(); i++) {
    QLineEdit *le = allLineEdits.at(i);
    QString leObjectName = le->objectName();
    QString leValue = le->text().trimmed();

    QDomElement widget = projectDoc->createElement("widget");
    widget.setAttribute("type", "LineEdit");
    item->appendChild(widget);
    
    QDomElement objectName = projectDoc->createElement("objectName");
    QDomText objectNameValue = projectDoc->createTextNode(leObjectName);
    objectName.appendChild(objectNameValue);
    widget.appendChild(objectName);

    QDomElement text = projectDoc->createElement("text");
    QDomText textValue = projectDoc->createTextNode(leValue);
    text.appendChild(textValue);
    widget.appendChild(text);
  }

  // Combo boxes:
  //--------------
  QList<QComboBox *> allComboBoxes = parentWidget->findChildren<QComboBox *>(); 
  
  for(int i = 0; i < allComboBoxes.size(); i++) {
    QComboBox *cb = allComboBoxes.at(i);
    QString cbObjectName = cb->objectName();
    QString cbValue = QString::number(cb->currentIndex());

    QDomElement widget = projectDoc->createElement("widget");
    widget.setAttribute("type", "ComboBox");
    item->appendChild(widget);
    
    QDomElement objectName = projectDoc->createElement("objectName");
    QDomText objectNameValue = projectDoc->createTextNode(cbObjectName);
    objectName.appendChild(objectNameValue);
    widget.appendChild(objectName);

    QDomElement currentIndex = projectDoc->createElement("currentIndex");
    QDomText currentIndexValue = projectDoc->createTextNode(cbValue);
    currentIndex.appendChild(currentIndexValue);
    widget.appendChild(currentIndex);
  }
}

void ProjectIO::readFromProject(QDomDocument *projectDoc, QDomElement *item)
{
}

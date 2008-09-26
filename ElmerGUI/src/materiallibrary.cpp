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
 *  ElmerGUI materiallibrary                                                 *
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
#include "materiallibrary.h"

using namespace std;

MaterialLibrary::MaterialLibrary(QWidget *parent)
  : QDialog(parent)
{
  ui.setupUi(this);

  connect(ui.okButton, SIGNAL(clicked()), this, SLOT(okButtonClicked()));

  // Load library:
  //--------------
  QString errStr;
  int errRow;
  int errCol;
  QFile materialFile("edf/egmaterials.xml");
  
  if(!materialFile.exists()) {
    QMessageBox::information(window(), tr("material loader"),
			     tr("Material library does not exist"));
    return;

  } else {  

    if(!materialDoc.setContent(&materialFile, true, &errStr, &errRow, &errCol)) {
      QMessageBox::information(window(), tr("Material loader"),
			       tr("Parse error at line %1, col %2:\n%3")
			       .arg(errRow).arg(errCol).arg(errStr));
      materialFile.close();
      return;
    }
  }

  materialFile.close();	
  
  if(materialDoc.documentElement().tagName() != "materiallibrary") {
    QMessageBox::information(window(), tr("Material loader"),
			     tr("This is not a material library file"));
    return;
  }

  // Update list widget:
  //---------------------
  QListWidget *list = ui.materialListWidget;
  list->clear();  
  QDomElement contents = materialDoc.documentElement();
  QDomElement material = contents.firstChildElement("material");
  for( ; !material.isNull(); material = material.nextSiblingElement()) {
    QString materialName = material.attribute("name");
    QListWidgetItem *item = new QListWidgetItem(materialName, list);
  }
  list->sortItems();

  // Enable selection by double clicking:
  //--------------------------------------
  connect(list, SIGNAL(itemDoubleClicked(QListWidgetItem*)), this, SLOT(itemDoubleClicked(QListWidgetItem*)));

  // list->setAlternatingRowColors(true);
}

MaterialLibrary::~MaterialLibrary()
{
}

void MaterialLibrary::okButtonClicked()
{
  QListWidget *list = ui.materialListWidget;
  QListWidgetItem *item = list->currentItem();

  if(item == NULL)
    return;
  
  // Clear all line edits:
  //-----------------------
  for(int i = 0; i < editor->hash.count(); i++) {
    hash_entry_t value = editor->hash.values().at(i);
    QDomElement elem = value.elem;
    QWidget *widget = value.widget;
    if(elem.attribute("Widget") == "Edit") {
      QLineEdit *lineEdit = (QLineEdit*)widget;
      lineEdit->setText("");
    }
  }

  // Update line edits with library properties:
  //--------------------------------------------
  QDomElement contents = materialDoc.documentElement();
  QDomElement material = contents.firstChildElement("material");
  for( ; !material.isNull(); material = material.nextSiblingElement()) {
    QString materialName = material.attribute("name");
    
    if(materialName != item->text())
      continue;

    editor->nameEdit->setText(materialName);
    
    QDomElement property = material.firstChildElement();
    for( ; !property.isNull(); property = property.nextSiblingElement()) {
      QString propertyName = property.attribute("name").trimmed().toLower();
      QString propertyValue = property.text().trimmed();

#if 0
      cout << string(materialName.toAscii()) << ": " 
	   << string(propertyName.toAscii()) << ": " 
	   << string(propertyValue.toAscii()) << endl;
#endif

      // Copy the parameter value into material editor:
      //------------------------------------------------
      for(int i = 0; i < editor->hash.count(); i++) {
	hash_entry_t value = editor->hash.values().at(i);
	QDomElement elem = value.elem;
	QWidget *widget = value.widget;
	QString widgetName = elem.firstChildElement("Name").text().trimmed().toLower();
	
	if(elem.attribute("Widget") == "Edit") {
	  QLineEdit *lineEdit = (QLineEdit*)widget;
	  if(propertyName == widgetName) 
	    lineEdit->setText(propertyValue);
	}
      }
    }
  }

  this->close();
  editor->raise();
}

void MaterialLibrary::itemDoubleClicked(QListWidgetItem *item)
{
  QListWidget *list = ui.materialListWidget;
  list->setCurrentItem(item);
  okButtonClicked();
}

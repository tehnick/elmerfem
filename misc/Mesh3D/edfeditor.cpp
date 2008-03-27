#include <QtGui>
#include <iostream>
#include "edfeditor.h"

using namespace std;

EdfEditor::EdfEditor(QWidget *parent)
  : QWidget(parent)
{
  addIcon = QIcon(":/icons/list-add.png");
  removeIcon = QIcon(":/icons/list-remove.png");

  setWindowFlags(Qt::Window);

  // Tree widget:
  //-------------
  edfTree = new QTreeWidget;
  edfTree->setColumnCount(2);

  QStringList qsl;
  qsl << "Tag" << "Value";
  edfTree->setHeaderLabels(qsl);

  // Buttons:
  //---------
  addButton = new QPushButton(tr("&Add"));
  addButton->setIcon(addIcon);
  connect(addButton, SIGNAL(clicked()), this, SLOT(addButtonClicked()));
  
  removeButton = new QPushButton(tr("&Remove"));
  removeButton->setIcon(removeIcon);
  connect(removeButton, SIGNAL(clicked()), this, SLOT(removeButtonClicked()));

  QHBoxLayout *buttonLayout = new QHBoxLayout;  
  buttonLayout->addWidget(addButton);
  buttonLayout->addWidget(removeButton);

  // Main layout:
  //-------------
  QVBoxLayout *mainLayout = new QVBoxLayout;
  mainLayout->addWidget(edfTree);
  mainLayout->addLayout(buttonLayout);
  setLayout(mainLayout);

}

//----------------------------------------------------------------------------
EdfEditor::~EdfEditor()
{
}

//----------------------------------------------------------------------------
void EdfEditor::insertEntry(QDomElement element,
			    QTreeWidgetItem *parentItem)
{
  if(element.isNull())
    return;
  
  QTreeWidgetItem *newItem = new QTreeWidgetItem(parentItem);
  
  newItem->setText(0, element.tagName().trimmed());
  
  if(element.childNodes().count() == 1)
    newItem->setText(1, element.text().trimmed());
  
  edfTree->addTopLevelItem(newItem);
  
  if(!element.firstChildElement().isNull()) 
    insertEntry(element.firstChildElement(), newItem);
  
  insertEntry(element.nextSiblingElement(), parentItem);      
}

//----------------------------------------------------------------------------
void EdfEditor::setupEditor(QDomDocument &elmerDefs)
{
  // get root entry
  root = elmerDefs.documentElement();
  element = root.firstChildElement("PDE");

  // recursively add entries to tree view
  insertEntry(element, NULL);

  this->show();
}

//----------------------------------------------------------------------------
QSize EdfEditor::minimumSizeHint() const
{
  return QSize(64, 64);
}

//----------------------------------------------------------------------------
QSize EdfEditor::sizeHint() const
{
  return QSize(720, 480);
}

//----------------------------------------------------------------------------
void EdfEditor::addButtonClicked()
{
  cout << "Edf editor: Add-button clicked" << endl;
  cout.flush();
  close();
}

//----------------------------------------------------------------------------
void EdfEditor::removeButtonClicked()
{
  cout << "Edf editor: Remove-button clicked" << endl;
  cout.flush();
  close();
}

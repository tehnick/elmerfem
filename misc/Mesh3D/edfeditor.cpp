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

  connect(edfTree, SIGNAL(itemClicked(QTreeWidgetItem*,int)),
	  this, SLOT(treeItemClicked(QTreeWidgetItem*,int)));

  edfTree->setColumnCount(2);
  
  edfTree->header()->setResizeMode(QHeaderView::Stretch);
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
  newItem->setFlags(newItem->flags() | Qt::ItemIsEditable);

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
  // get root entry & recursively add all entries to the tree:
  root = elmerDefs.documentElement();
  element = root.firstChildElement("PDE");
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
  return QSize(480, 320);
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

//----------------------------------------------------------------------------
void EdfEditor::treeItemClicked(QTreeWidgetItem *item, int column)
{
  cout << "Item clicked: ";
  cout << string(item->text(column).trimmed().toAscii());
  cout << endl;
  cout.flush();
}

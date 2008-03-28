#include <QtGui>
#include <iostream>
#include "edfeditor.h"

using namespace std;

EdfEditor::EdfEditor(QWidget *parent)
  : QWidget(parent)
{
  addIcon = QIcon(":/icons/list-add.png");
  removeIcon = QIcon(":/icons/list-remove.png");
  saveAsIcon = QIcon(":/icons/document-save.png");
  applyIcon = QIcon(":/icons/dialog-close.png");

  setWindowFlags(Qt::Window);

  // Tree widget:
  //-------------
  edfTree = new QTreeWidget;

  connect(edfTree, SIGNAL(itemClicked(QTreeWidgetItem*,int)),
	  this, SLOT(treeItemClicked(QTreeWidgetItem*,int)));

  edfTree->setColumnCount(3);
  edfTree->setColumnWidth(0,200);
  edfTree->setColumnWidth(1,200);
  edfTree->setColumnWidth(2,200);

  //edfTree->header()->setResizeMode(QHeaderView::Stretch);

  QStringList qsl;
  qsl << "Tag" << "Attributes" << "Value";
  edfTree->setHeaderLabels(qsl);
  edfTree->setAlternatingRowColors(true);

  // Buttons:
  //---------
  addButton = new QPushButton(tr("&Add child"));
  addButton->setIcon(addIcon);
  connect(addButton, SIGNAL(clicked()), this, SLOT(addButtonClicked()));
  
  removeButton = new QPushButton(tr("&Remove item"));
  removeButton->setIcon(removeIcon);
  connect(removeButton, SIGNAL(clicked()), this, SLOT(removeButtonClicked()));

  saveAsButton = new QPushButton(tr("&Save as"));
  saveAsButton->setIcon(saveAsIcon);
  connect(saveAsButton, SIGNAL(clicked()), this, SLOT(saveAsButtonClicked()));

  applyButton = new QPushButton(tr("&Close"));
  applyButton->setIcon(applyIcon);
  connect(applyButton, SIGNAL(clicked()), this, SLOT(applyButtonClicked()));

  QHBoxLayout *buttonLayout = new QHBoxLayout;  
  buttonLayout->addWidget(addButton);
  buttonLayout->addWidget(removeButton);
  buttonLayout->addWidget(saveAsButton);
  buttonLayout->addWidget(applyButton);

  // Main layout:
  //-------------
  QVBoxLayout *mainLayout = new QVBoxLayout;
  mainLayout->addWidget(edfTree);
  mainLayout->addLayout(buttonLayout);
  setLayout(mainLayout);

  setWindowTitle("Elmer Definitions File editor");
}

//----------------------------------------------------------------------------
EdfEditor::~EdfEditor()
{
}

//----------------------------------------------------------------------------
void EdfEditor::insertItem(QDomElement element,
			   QTreeWidgetItem *parentItem)
{
  if(element.isNull())
    return;

  // set expanded
  if(parentItem != NULL)
    parentItem->setExpanded(true);

  // create new tree item
  QTreeWidgetItem *newItem = new QTreeWidgetItem(parentItem);

  newItem->setText(0, element.tagName().trimmed());
  newItem->setFlags(newItem->flags() | Qt::ItemIsEditable);

  // display element attributes and value
  if(element.firstChildElement().isNull()) {

    // display attributes
    QStringList list;
    QDomNamedNodeMap attributeMap = element.attributes();
    for(int index = 0; index < attributeMap.count(); index++) {
      QDomNode attribute = attributeMap.item(index);
      list << attribute.nodeName() + "=\"" + attribute.nodeValue() + "\"";
    }
    newItem->setText(1, list.join(" "));

    // diaplay value
    newItem->setText(2, element.text().split("\n").join(" ").trimmed());
  }
  
  // update hash
  elementForItem.insert(newItem, element);

  // add item
  edfTree->addTopLevelItem(newItem);
  
  if(!element.firstChildElement().isNull()) 
    insertItem(element.firstChildElement(), newItem);
  
  insertItem(element.nextSiblingElement(), parentItem);      
}

//----------------------------------------------------------------------------
void EdfEditor::setupEditor(QDomDocument &elmerDefs)
{
  this->elmerDefs = &elmerDefs;

  // get root entry & recursively add all entries to the tree:

  disconnect(edfTree, SIGNAL(itemChanged(QTreeWidgetItem*, int)),
	     this, SLOT(updateElement(QTreeWidgetItem*, int)));

  edfTree->clear();
  root = elmerDefs.documentElement();
  insertItem(root, NULL);
  edfTree->setCurrentItem(NULL);

  connect(edfTree, SIGNAL(itemChanged(QTreeWidgetItem*, int)),
	  this, SLOT(updateElement(QTreeWidgetItem*, int)));

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
void EdfEditor::updateElement(QTreeWidgetItem *item, int column)
{
  // get element from hash
  QDomElement oldElement = elementForItem.value(item);

  if(oldElement.isNull())
    return;
  
  // create new element
  QDomElement newElement = elmerDefs->createElement(item->text(0));

  // set new attributes
  QStringList list = item->text(1).trimmed().split(" ");

  for(int i = 0; i < list.size(); i++) {
    QString qs = list.at(i).trimmed();
    QStringList qsl = qs.split("=");

    if(qsl.size() < 2)
      break;

    QString attribute = qsl.at(0).trimmed();
    QString attributeValue = qsl.at(1).trimmed();
    newElement.setAttribute(attribute, attributeValue);
  }
  
  // set new value
  QDomText text = elmerDefs->createTextNode(item->text(2));
  newElement.appendChild(text);

  // replace old element with new
  QDomElement parentElement = elementForItem.value(item->parent());
  parentElement.replaceChild(newElement, oldElement);  

  // update hash
  elementForItem.insert(item, newElement);  
}

//----------------------------------------------------------------------------
void EdfEditor::addButtonClicked()
{
  QTreeWidgetItem *current = edfTree->currentItem();
  
  if(current == NULL)
    return;

  QString newTag = "empty";
  QString newAttribute = "attribute";
  QString newAttributeValue = "empty";
  QString newValue = "empty";
  
  // add to tree:
  QTreeWidgetItem *newItem = new QTreeWidgetItem(current);

  newItem->setFlags(newItem->flags() | Qt::ItemIsEditable);

  newItem->setText(0, newTag);
  newItem->setText(1, newAttribute + "=\"" + newAttributeValue + "\"");
  newItem->setText(2, newValue);
  current->addChild(newItem);
  newItem->parent()->setExpanded(true);

  // add to document
  QDomElement newElement = elmerDefs->createElement(newTag);
  newElement.setAttribute(newAttribute, newAttributeValue);

  QDomText newText = elmerDefs->createTextNode(newValue);
  newElement.appendChild(newText);

  QDomElement parent = elementForItem.value(newItem->parent());
  parent.appendChild(newElement);

  // update hash
  elementForItem.insert(newItem, newElement);
}

//----------------------------------------------------------------------------
void EdfEditor::removeButtonClicked()
{
  QTreeWidgetItem *currentItem = edfTree->currentItem();

  if(currentItem == NULL)
    return;

  QTreeWidgetItem *parentItem = currentItem->parent();
  QDomElement element = elementForItem.value(currentItem);
  QDomElement parentElement = elementForItem.value(parentItem);

  parentItem->removeChild(currentItem);
  parentElement.removeChild(element);

  // update hash
  elementForItem.remove(currentItem);
}

//----------------------------------------------------------------------------
void EdfEditor::saveAsButtonClicked()
{
  cout << "Saving definitions..." << endl;
  cout.flush();
  
  QString fileName;

  fileName = QFileDialog::getSaveFileName(this, tr("Save definitions"), "", tr("EDF (*.xml)") );

  if(fileName.isEmpty())
    return;

  const int indent = 3;
  
  QFile file;
  file.setFileName(fileName);
  file.open(QIODevice::WriteOnly);
  QTextStream out(&file);

  elmerDefs->save(out, indent);
}

//----------------------------------------------------------------------------
void EdfEditor::applyButtonClicked()
{
  this->close();
}

//----------------------------------------------------------------------------
void EdfEditor::treeItemClicked(QTreeWidgetItem *item, int column)
{
  return;

  cout << "Item clicked: ";
  cout << string(item->text(column).trimmed().toAscii());
  cout << endl;

  // test hash:
  QDomElement element = elementForItem.value(item);
  QString qs = element.tagName().trimmed();
  cout << "element tag name from hash: " << string(qs.toAscii()) << " ";
  qs = element.attribute("attribute").trimmed();
  cout << "Attribute=\"" << string(qs.toAscii()) << "\" ";
  cout.flush();
  qs = element.text().trimmed();
  cout << "Value=\"" << string(qs.toAscii()) << "\"" << endl;
  cout.flush();
}


// TODO: make edited changes in document

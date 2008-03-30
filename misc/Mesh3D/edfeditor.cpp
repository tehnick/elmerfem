#include <QtGui>
#include <iostream>
#include "edfeditor.h"

using namespace std;

EdfEditor::EdfEditor(QWidget *parent)
  : QWidget(parent)
{
  addIcon = QIcon(":/icons/list-add.png");
  removeIcon = QIcon(":/icons/list-remove.png");
  openIcon = QIcon(":/icons/document-open.png");
  saveAsIcon = QIcon(":/icons/document-save.png");
  applyIcon = QIcon(":/icons/dialog-close.png");

  lastActive = NULL;
  ctrlPressed = false;

  // Set up tree widget:
  //--------------------
  edfTree = new QTreeWidget;

  connect(edfTree, SIGNAL(itemClicked(QTreeWidgetItem*,int)),
	  this, SLOT(treeItemClicked(QTreeWidgetItem*,int)));

  edfTree->setColumnCount(3);
  edfTree->setColumnWidth(0,200);
  edfTree->setColumnWidth(1,200);
  edfTree->setColumnWidth(2,200);

  edfTree->setAnimated(true);

  // edfTree->header()->setResizeMode(QHeaderView::Stretch);

  // Set internal drag'n drop mode on:
  //----------------------------------
  // edfTree->setDragEnabled(true);
  // edfTree->setDragDropMode(QAbstractItemView::InternalMove);
  // edfTree->setDropIndicatorShown(true);
  // edfTree->setDragDropOverwriteMode(false);

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

  openButton = new QPushButton(tr("&Open"));
  openButton->setIcon(openIcon);
  connect(openButton, SIGNAL(clicked()), this, SLOT(openButtonClicked()));

  saveAsButton = new QPushButton(tr("&Save as"));
  saveAsButton->setIcon(saveAsIcon);
  connect(saveAsButton, SIGNAL(clicked()), this, SLOT(saveAsButtonClicked()));

  applyButton = new QPushButton(tr("&Close"));
  applyButton->setIcon(applyIcon);
  connect(applyButton, SIGNAL(clicked()), this, SLOT(applyButtonClicked()));

  QHBoxLayout *buttonLayout = new QHBoxLayout;  
  buttonLayout->addWidget(addButton);
  buttonLayout->addWidget(removeButton);
  buttonLayout->addWidget(openButton);
  buttonLayout->addWidget(saveAsButton);
  buttonLayout->addWidget(applyButton);

  // Main layout:
  //-------------
  QVBoxLayout *mainLayout = new QVBoxLayout;
  mainLayout->addWidget(edfTree);
  mainLayout->addLayout(buttonLayout);
  setLayout(mainLayout);

  setWindowTitle("Elmer Definitions File editor");

  setFocusPolicy(Qt::ClickFocus);
}

//----------------------------------------------------------------------------
EdfEditor::~EdfEditor()
{
}

//----------------------------------------------------------------------------
void EdfEditor::insertItemForElement(QDomElement element,
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

  
  // display attributes
  QStringList list;
  QDomNamedNodeMap attributeMap = element.attributes();
  for(int index = 0; index < (int)attributeMap.length(); index++) {
    QDomNode attribute = attributeMap.item(index);
    list << attribute.nodeName() + "=\"" + attribute.nodeValue() + "\"";
  }
  newItem->setText(1, list.join(" "));
  
  // display value
  if(element.firstChildElement().isNull()) 
    newItem->setText(2, element.text().split("\n").join(" ").trimmed());
  
  // update hash
  elementForItem.insert(newItem, element);

  // add item
  edfTree->addTopLevelItem(newItem);
  
  if(!element.firstChildElement().isNull()) 
    insertItemForElement(element.firstChildElement(), newItem);
  
  insertItemForElement(element.nextSiblingElement(), parentItem);      
}

//----------------------------------------------------------------------------
void EdfEditor::setupEditor(QDomDocument &elmerDefs)
{
  this->elmerDefs = &elmerDefs;

  disconnect(edfTree, SIGNAL(itemChanged(QTreeWidgetItem*, int)),
	     this, SLOT(updateElement(QTreeWidgetItem*, int)));

  // clear hash
  elementForItem.clear(); 

  // get root entry & recursively add all children to the tree:
  edfTree->clear();
  QDomElement root = elmerDefs.documentElement();
  insertItemForElement(root, NULL);
  edfTree->setCurrentItem(NULL);

  connect(edfTree, SIGNAL(itemChanged(QTreeWidgetItem*, int)),
	  this, SLOT(updateElement(QTreeWidgetItem*, int)));

  edfTree->setCurrentItem(NULL);
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
  QDomElement element = elementForItem.value(item);

  if(element.isNull())
    return;

  // set new tag
  element.setTagName(item->text(0).trimmed());

  // delete old attributes
  QDomNamedNodeMap oldAttributes = element.attributes();
  for(int i = 0; i<(int)oldAttributes.length(); i++) {
    QDomNode node = oldAttributes.item(i);
    QString name = node.nodeName();
    element.removeAttribute(name);
  }
  
  // parse and set new attributes
  QString pattern = "([a-zA-Z0-9]+)[ \t]*=[ \t]*[\"]([^\"]+)[\"]";
  QRegExp expression(pattern);
  QString qs = item->text(1).trimmed();
  int index = qs.indexOf(expression);
  QString parsedString = "";
  if(index < 0)
    parsedString = qs;

  while(index >= 0) {
    int length = expression.matchedLength();
    QString currentMatch = qs.mid(index, length);
    QStringList currentList = currentMatch.split("=");
    QString name = currentList.at(0);
    QString value = currentList.at(1);

    int firstPar = value.indexOf("\"", 0);
    int secondPar = value.indexOf("\"", firstPar+1);
    value = value.mid(firstPar+1, secondPar-firstPar-1);

    // cout << string(name.toAscii()) << " " << string(value.toAscii()) << endl;
    // cout.flush();

    parsedString.append(name + "=\"" + value + "\" ");

    element.setAttribute(name.trimmed(), value.trimmed());
    index = qs.indexOf(expression, index + length);
  }

  // update display with parsed attributes
  item->setText(1, parsedString);

  // set new text (only if old element has no children)
  if(element.firstChildElement().isNull()) {

    // remove old text node
    QDomNodeList children = element.childNodes();
    for(int i=0;  i<(int)children.length(); i++) {
      QDomNode node = children.at(i);
      if(node.isText())
	element.removeChild(node);
    }
    
    // new text node
    QDomText text = elmerDefs->createTextNode(item->text(2));
    element.appendChild(text);
    
  } else {
    
    // clear value from tree view to avoid confusions:
    item->setText(2, "");
  }

  // no need to update hash
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

  // clear the value field for current item (as it just became parent)
  current->setText(2, "");

  // add to document
  QDomElement newElement = elmerDefs->createElement(newTag);
  newElement.setAttribute(newAttribute, newAttributeValue);

  QDomText newText = elmerDefs->createTextNode(newValue);
  newElement.appendChild(newText);

  QDomElement parent = elementForItem.value(newItem->parent());
  parent.appendChild(newElement);

  // update hash
  elementForItem.insert(newItem, newElement);

  edfTree->setCurrentItem(newItem);
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

  edfTree->setCurrentItem(NULL);
}

//----------------------------------------------------------------------------
void EdfEditor::saveAsButtonClicked()
{
  QString fileName;

  fileName = QFileDialog::getSaveFileName(this,
                 tr("Save definitions"), "", tr("EDF (*.xml)") );

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
void EdfEditor::openButtonClicked()
{
  QString fileName;

  fileName = QFileDialog::getOpenFileName(this, 
	      tr("Open definitions"), "", tr("EDF (*.xml)") );

  if(fileName.isEmpty())
    return;

  QFile file;
  file.setFileName(fileName);
  file.open(QIODevice::ReadOnly);

  QString errStr;
  int errRow;
  int errCol;

  if(!elmerDefs->setContent(&file, true, &errStr, &errRow, &errCol)) {
    QMessageBox::information(window(), tr("Elmer definitions file"),
			     tr("Parse error at line %1, col %2:\n%3")
			     .arg(errRow).arg(errCol).arg(errStr));
    file.close();
    return;

  } else {
      
    if(elmerDefs->documentElement().tagName() != "edf") {
      QMessageBox::information(window(), tr("Elmer definitions file"),
			       tr("This is not an edf file"));
      delete elmerDefs;
      file.close();
      return;
      
    }
  }
  
  setupEditor(*elmerDefs);

  edfTree->setCurrentItem(NULL);
}


//----------------------------------------------------------------------------
void EdfEditor::applyButtonClicked()
{
  this->close();
}

// Swap the place of two items...
//----------------------------------------------------------------------------
void EdfEditor::treeItemClicked(QTreeWidgetItem *item, int column)
{
  if(item == lastActive)
    return;

  if(lastActive == NULL) {
    lastActive = item;
    return;
  }

  if(!ctrlPressed)
    return;

  // items must have the same parent:
  if(item->parent() != lastActive->parent()) {
    cout << "Items do not belong to the same parent - unable to swap" << endl;
    cout.flush();
    lastActive = item;
    return;
  }

  // get elements:
  QDomElement element = elementForItem.value(item);  
  QDomElement lastActiveElement = elementForItem.value(lastActive);  

  // elements must have the same parent (should always be true):
  if(element.parentNode() != lastActiveElement.parentNode()) {
    cout << "Parent node mismatch - unable to swap items" << endl;
    cout.flush();
    lastActive = item;
    return;
  }
  
  // clone elements:
  QDomNode clone = element.cloneNode(true);
  QDomNode lastActiveClone = lastActiveElement.cloneNode(true);

  // cross replace elements with their clones:
  element.parentNode().replaceChild(lastActiveClone, element);
  lastActiveElement.parentNode().replaceChild(clone, lastActiveElement);

  // remove elements from the document:
  element.parentNode().removeChild(element);
  lastActiveElement.parentNode().removeChild(lastActiveElement);

  // make sure that old elements are cleared (they should be already):
  element.clear();
  lastActiveElement.clear();

  // rebuild tree & hash:
  setupEditor(*elmerDefs);

  // set focus back to the last selected item:
  lastActive = NULL;
  for(int i = 0; i < elementForItem.count(); i++) {
    if(elementForItem.values().at(i) == lastActiveClone) 
      edfTree->setCurrentItem(elementForItem.keys().at(i));
  }

  return;
}

// Key pressed...
//-----------------------------------------------------------------------------
void EdfEditor::keyPressEvent(QKeyEvent *event)
{
  if(event->key() == Qt::Key_Control)
    ctrlPressed = true;
}


// Key released...
//-----------------------------------------------------------------------------
void EdfEditor::keyReleaseEvent(QKeyEvent *event)
{
  if(event->key() == Qt::Key_Control)
    ctrlPressed = false;
}

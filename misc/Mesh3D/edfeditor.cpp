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
  edfTree->setColumnCount(1);

  QStringList qsl;
  qsl << "Equation"; 
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
void EdfEditor::setupEditor(QDomDocument &elmerDefs)
{
  class Test {
  public:
    void printStuff(QDomElement e, int level) {

      if(e.isNull())
	return;
      
      for(int i=0; i<level; i++) cout << "  ";

      QString qs1 = e.tagName().trimmed();
      cout << "tag=" << string(qs1.toAscii()) << endl;
      
      QDomNodeList nl = e.childNodes();

      if(nl.count() == 1) {
	for(int i=0; i<level; i++) cout << "  ";

	QString qs2 = e.text().trimmed();
	cout << "  " << string(qs2.toAscii()) << endl;
	cout.flush();
      }
      
      if(!e.firstChildElement().isNull()) 
	printStuff(e.firstChildElement(), level+1);
      
      printStuff(e.nextSiblingElement(), level);
      
    }
  };

  Test test;

  // Get root element of elmerDefs:
  //-------------------------------
  root = elmerDefs.documentElement();
  element = root.firstChildElement();
  int level = 0;

  while(!element.isNull()) {
    test.printStuff(element, level);
    element = element.nextSiblingElement();
  }

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
  return QSize(400, 300);
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

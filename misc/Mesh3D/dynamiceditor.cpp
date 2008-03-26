#include <QtGui>
#include <iostream>
#include "dynamiceditor.h"

using namespace std;

DynamicEditor::DynamicEditor(QWidget *parent)
  : QWidget(parent)
{
  QDomElement element;
  QDomElement name;
  QDomElement material;
  QDomElement param;

  tab_t *t;
  field_t *f;

  addIcon = QIcon(":/icons/list-add.png");
  removeIcon = QIcon(":/icons/list-remove.png");

  setWindowFlags(Qt::Window);

  // Read in edf file in xml-format:
  //--------------------------------
  QFile file("edf.xml");
  
  if(!domDocument.setContent(&file, true, &errorStr, &errorLine, &errorColumn))
    QMessageBox::information(window(), tr("Dummy editor"),
			     tr("Parse error at line %1, column %2:\n%3")
			     .arg(errorLine).arg(errorColumn).arg(errorStr));
  
  QDomElement root = domDocument.documentElement();
  
  if(root.tagName() != "edf")
    QMessageBox::information(window(), tr("Dummy editor"),
			     tr("This is not an edf file"));

  // Count equations
  //----------------
  element = root.firstChildElement("PDE");
  int equations = 0;
  while(element.text() != "") {
    element = element.nextSiblingElement();
    equations++;
  }

  // Set up tab data:
  //------------------
  tabs = equations;
  tab = new tab_t[tabs];

  element = root.firstChildElement("PDE");
  equations = 0;
  while(element.text() != "") {

    name = element.firstChildElement("Name");
    material = element.firstChildElement("Material");
    param = material.firstChildElement("Parameter");
    
    // count material params
    int count = 0;
    while(param.text() != "") {
      count++;
      param = param.nextSiblingElement();
    }
    
    t = &tab[equations++];
    t->name = name.text().trimmed();
    t->fields = count;
    t->field = new field_t[t->fields];
    
    param = material.firstChildElement("Parameter");
    count = 0;
    while(param.text() != "") {
      f = &t->field[count++];
      f->type = EDIT_FIELD;
      f->label = param.text().trimmed();
      f->editDefault = "";
      param = param.nextSiblingElement();
    }
    
    element = element.nextSiblingElement();
  }
  
  // Set up tabs:
  //-------------
  tabWidget = new QTabWidget;
  
  for(int i = 0; i < tabs; i++) {
    t = &tab[i];

    QGridLayout *grid = new QGridLayout;

    for(int j = 0; j < t->fields; j++) {
      f = &t->field[j];
      
      QLabel *label = new QLabel;
      label->setText(f->label);
      grid->addWidget(label, j, 0);
      
      if(f->type == EDIT_FIELD) {
	QLineEdit *edit = new QLineEdit;
	edit->setText(f->editDefault);
	grid->addWidget(edit, j, 1);
      }
      
      if(f->type == COMBO_FIELD) {
	QComboBox *combo = new QComboBox;
	for(int k = 0; k < f->comboEntries; k++)
	  combo->addItem(f->comboEntry[k]);
	grid->addWidget(combo, j, 1);
      }
    }
    
    // add a dummy frame
    QFrame *dummyFrame = new QFrame;
    grid->addWidget(dummyFrame, t->fields, 0);
    
    // put grid in a group/frame
    // QGroupBox *group = new QGroupBox;
    // group->setLayout(grid);
    QFrame *frm = new QFrame;
    frm->setLayout(grid);

    // finally, add group/frame to tab
    // tabWidget->addTab(group, t->name);
    tabWidget->addTab(frm, t->name);
  }
  
  // Buttons:
  //----------
  addButton = new QPushButton(tr("&Add"));
  addButton->setIcon(addIcon);
  connect(addButton, SIGNAL(clicked()),
	  this, SLOT(addButtonClicked()));
  
  removeButton = new QPushButton(tr("&Remove"));
  removeButton->setIcon(removeIcon);
  connect(removeButton, SIGNAL(clicked()),
	  this, SLOT(removeButtonClicked()));

  QHBoxLayout *buttonLayout = new QHBoxLayout;  
  buttonLayout->addWidget(addButton);
  buttonLayout->addWidget(removeButton);

  // Main layout:
  //-------------
  QVBoxLayout *mainLayout = new QVBoxLayout;
  mainLayout->addWidget(tabWidget);
  mainLayout->addLayout(buttonLayout);
  setLayout(mainLayout);

  // Window title:
  //---------------
  setWindowTitle(tr("Dynamic editor"));
}

//----------------------------------------------------------------------------
DynamicEditor::~DynamicEditor()
{
  delete [] tab;
}

//----------------------------------------------------------------------------
QSize DynamicEditor::minimumSizeHint() const
{
  return QSize(64, 64);
}

//----------------------------------------------------------------------------
QSize DynamicEditor::sizeHint() const
{
  return QSize(400, 300);
}

//----------------------------------------------------------------------------
void DynamicEditor::addButtonClicked()
{
  cout << "Dynamic editor: Add-button clicked" << endl;
  cout.flush();
  close();
}

//----------------------------------------------------------------------------
void DynamicEditor::removeButtonClicked()
{
  cout << "Dynamic editor: Remove-button clicked" << endl;
  cout.flush();
  close();
}

#include <QtGui>
#include <iostream>
#include "dynamiceditor.h"

using namespace std;

DynamicEditor::DynamicEditor(QWidget *parent)
  : QWidget(parent)
{
  setWindowFlags(Qt::Window);

  addIcon = QIcon(":/icons/list-add.png");
  removeIcon = QIcon(":/icons/list-remove.png");

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

  // Set up tab data:
  //------------------
  QDomElement element;
  QDomElement name;
  QDomElement material;
  QDomElement param;

  tab_t *t;
  field_t *f;

  tabs = 2;
  tab = new tab_t[tabs];
  
  // FIRST PAGE
  element = root.firstChildElement("PDE");
  name = element.firstChildElement("Name");
  material = element.firstChildElement("Material");

  t = &tab[0];
  t->name = name.text().trimmed();
  t->fields = 2; // assumption
  t->field = new field_t[t->fields];

  param = material.firstChildElement("Parameter");
  f = &t->field[0];
  f->type = EDIT_FIELD;
  f->label = param.text().trimmed();
  f->editDefault = "0.1";

  param = param.nextSiblingElement("Parameter");
  f = &t->field[1];
  f->type = EDIT_FIELD;
  f->label = param.text().trimmed();
  f->editDefault = "";

  // SECOND PAGE
  element = element.nextSiblingElement("PDE");
  name = element.firstChildElement("Name");
  material = element.firstChildElement("Material");

  t = &tab[1];
  t->name = name.text().trimmed();
  t->fields = 3; // assumption
  t->field = new field_t[t->fields];

  param = material.firstChildElement("Parameter");
  f = &t->field[0];
  f->type = EDIT_FIELD;
  f->label = param.text().trimmed();
  f->editDefault = "3";

  param = param.nextSiblingElement("Parameter");
  f = &t->field[1];
  f->type = COMBO_FIELD;
  f->label = param.text().trimmed();
  f->comboEntries = 5;
  f->comboEntry = new QString[f->comboEntries];
  f->comboEntry[0] = "first";
  f->comboEntry[1] = "second";
  f->comboEntry[2] = "apple";
  f->comboEntry[3] = "cat";
  f->comboEntry[4] = "fifth";

  param = param.nextSiblingElement("Parameter");
  f = &t->field[2];
  f->type = EDIT_FIELD;
  f->label = param.text().trimmed();
  f->editDefault = "-3.14159";

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
    
    // put grid in a group
    QGroupBox *group = new QGroupBox;
    group->setLayout(grid);
    group->setTitle("Parameters");

    // finally, add group to tab
    tabWidget->addTab(group, t->name);
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

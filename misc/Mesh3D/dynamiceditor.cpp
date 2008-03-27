#include <QtGui>
#include <iostream>
#include "dynamiceditor.h"

using namespace std;

DynamicEditor::DynamicEditor(QWidget *parent)
  : QWidget(parent)
{
  addIcon = QIcon(":/icons/list-add.png");
  removeIcon = QIcon(":/icons/list-remove.png");
  setWindowFlags(Qt::Window);
}

//----------------------------------------------------------------------------
DynamicEditor::~DynamicEditor()
{
}

//----------------------------------------------------------------------------
void DynamicEditor::setupTabs(QDomDocument &elmerDefs)
{
  // Get root element of elmerDefs:
  //-------------------------------
  root = elmerDefs.documentElement();

  // count equations (tabs)
  tabs = 0;
  element = root.firstChildElement("PDE");
  while(!element.isNull()) {
    tabs++;
    element = element.nextSiblingElement();
  }

  // Set up tabs:
  //--------------
  tabWidget = new QTabWidget;
  tabWidget->setTabShape(QTabWidget::Triangular);

fprintf( stderr, "eh %d ?\n", tabs );
  tabs = 0;
  element = root.firstChildElement("PDE");
  while(!element.isNull()) {

    name = element.firstChildElement("Name");
    material = element.firstChildElement("Material");
    param = material.firstChildElement("Parameter");
    
    // count mat params
    int params = 0;
    while(!param.isNull()) {
      params++;
      param = param.nextSiblingElement();
    }
fprintf( stderr, "%d\n", params );
    
    QGridLayout *grid = new QGridLayout;

    params = 0;
    param = material.firstChildElement("Parameter");
    while(!param.isNull()) {

      // label
      QLabel *label = new QLabel;

      QString Name = param.attribute( "Name", "" );
      QString sifName   = param.attribute( "SifName", Name );
      QString nodeType  = param.attribute( "Type", "Edit" );
      QString paramType = param.attribute( "NameType", "Real" );
      QString paramDefault = param.attribute( "DefaultValue", "" );

cout << "Name: " << string(Name.toAscii()) << endl;
cout << "type: " << string(nodeType.toAscii()) << endl;
cout << "param type: " << string(paramType.toAscii()) << endl;
cout << "sifname: " << string(sifName.toAscii()) << endl;
cout << "+" << endl;

      label->setText(Name);
      grid->addWidget(label, params, 0);

      // line edit
      if ( nodeType == "Edit" ) 
      {
        QLineEdit *edit = new QLineEdit;
        edit->setText(paramDefault);
        grid->addWidget(edit, params, 1);
      } else if (nodeType == "Combo" ) {
        QComboBox *combo = new QComboBox;

        combo->setObjectName(Name);
        int count = 0, active=0;
        QDomElement item = param.firstChildElement("Item");
        while( !item.isNull() ) {
          QString itemType = item.attribute( "Type", "" );
          if ( itemType == "Active" ) active=count;
          combo->insertItem(count++,item.text().trimmed() );
          item = item.nextSiblingElement();
        } 
        combo->setCurrentIndex(active);
        grid->addWidget(combo, params, 1);
      } else if ( nodeType == "Logical" ) {
        QCheckBox *l = new QCheckBox;
        l->setText("");
        l->setChecked(false);
        if ( paramDefault == "true" ) {
          l->setChecked(true);
        }
        grid->addWidget(l, params, 1);
      }

      params++;
      param = param.nextSiblingElement();
    }

    // add a dummy frame for stretching
    QFrame *dummyFrame = new QFrame;
    grid->addWidget(dummyFrame, params, 0);
    
    // put grid in a frame
    QFrame *frm = new QFrame;
    frm->setLayout(grid);

    // add frame to tab
    tabWidget->addTab(frm, name.text().trimmed());
    
    tabs++;
    element = element.nextSiblingElement();
  }

  // Buttons:
  //----------
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
  mainLayout->addWidget(tabWidget);
  mainLayout->addLayout(buttonLayout);
  setLayout(mainLayout);

  // Window title:
  //---------------
  setWindowTitle(tr("Dynamic editor"));
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

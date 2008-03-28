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

class hash_entry_t
{
public:
  QObject *obj;
  QDomElement elem;
} h;

QHash<QString, hash_entry_t>  hash;
//QHash<QString, QObject *>   hash;
//QHash<QString, QDomElement> qhash;


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
    
    QGridLayout *grid = new QGridLayout;

    params = 0;
    param = material.firstChildElement("Parameter");

    for( ; !param.isNull(); param=param.nextSiblingElement(), params++ ) {

      // label
      QLabel *label = new QLabel;

      QString widget_type  = param.attribute( "Widget", "Edit" );

      QString paramType    = param.firstChildElement( "Type" ).text().trimmed();

      QString labelName = param.firstChildElement( "Name" ).text().trimmed();
      QString sifName      = param.firstChildElement( "SifName" ).text().trimmed();
      if ( sifName == "" ) sifName = labelName;

      QString paramDefault = param.firstChildElement( "DefaultValue").text().trimmed();

      QString whatis       = param.firstChildElement( "Whatis").text().trimmed();
      QString toolTip      = param.firstChildElement( "ToolTip").text().trimmed();
      QString statusTip    = param.firstChildElement( "StatusTip").text().trimmed();

      label->setText(labelName);
      grid->addWidget(label, params, 0);

      h.obj = NULL;
      // line edit
      if ( widget_type == "Edit" ) {
        QLineEdit *edit = new QLineEdit;
        h.obj = edit;

        edit->setText(paramDefault);
        edit->setWhatsThis(whatis);
        edit->setStatusTip(statusTip);

        grid->addWidget(edit, params, 1);

      } else if ( widget_type == "Combo" ) {
        QComboBox *combo = new QComboBox;
        h.obj = combo;

        combo->setObjectName(labelName);
        int count = 0, active=0;

        QDomElement item = param.firstChildElement("Item");
        for( ; !item.isNull(); item=item.nextSiblingElement() ) {
          QString itemType = item.attribute( "Type", "" );
          if ( itemType == "Active" ) active=count;
          combo->insertItem(count++,item.text().trimmed() );
        } 
        combo->setCurrentIndex(active);
        combo->setWhatsThis(whatis);
        combo->setStatusTip(statusTip);

        grid->addWidget(combo, params, 1);

      } else if ( widget_type == "CheckBox" ) {

        QCheckBox *l = new QCheckBox;
        h.obj = l;

        l->setText("");
        l->setChecked(false);
        if ( paramDefault == "true" ) l->setChecked(true);


        l->setWhatsThis(whatis);
        l->setStatusTip(statusTip);

        connect(l, SIGNAL(stateChanged(int)), this, SLOT(lSlot(int)));
        grid->addWidget(l, params, 1);
      }

      if ( h.obj ) {
        QString q = "/"+name.text().trimmed()+"/Material/"+labelName;
        h.obj->setProperty( "dom address",q);
        h.elem=param;
        hash[q] = h;
      }
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


void DynamicEditor::lSlot(int state)
{
  QDomElement param;
  QString q = QObject::sender()->property("dom address").toString();

  param = hash[q].elem.firstChildElement( "Activate" );
  for( ;!param.isNull(); param=param.nextSiblingElement("Activate") ) {
    q = param.text().trimmed();
    ((QWidget *)hash[q].obj)->setEnabled(state);
  }
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

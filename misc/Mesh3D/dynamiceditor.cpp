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
  QWidget *widget;
  QDomElement elem;
} h;

QHash<QString, hash_entry_t>  hash;


//----------------------------------------------------------------------------
void DynamicEditor::setupTabs(QDomDocument &elmerDefs, QString Section, int ID)
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


  tabWidget = new QTabWidget;
  tabWidget->setTabShape(QTabWidget::Triangular);

  tabs = 0;
  all_stuff = root.firstChildElement("ALL");
  element   = root.firstChildElement("PDE");

  while(!element.isNull()) {

    name = element.firstChildElement("Name");

    QGridLayout *grid = new QGridLayout;
  
    int params = 0;

    for( int iter=0; iter<2; iter++ )
    {
      if ( iter==0 )
        section = all_stuff.firstChildElement(Section);
      else 
        section = element.firstChildElement(Section);

      param = section.firstChildElement("Parameter");
      
      for( ;!param.isNull(); param=param.nextSiblingElement(), params++ ) {

        // label
        QString widget_type = param.attribute("Widget","Edit");

        QString paramType = param.firstChildElement("Type").text().trimmed();

        QString labelName = param.firstChildElement("Name").text().trimmed();
        QString sifName   = param.firstChildElement("SifName").text().trimmed();
        if ( sifName == "" ) sifName = labelName;

        QString paramDefault = param.firstChildElement("DefaultValue").text().trimmed();

        QString whatis    = param.firstChildElement("Whatis").text().trimmed();
        QString statusTip = param.firstChildElement("StatusTip").text().trimmed();

        h.widget = NULL;
        if ( widget_type == "Edit" ) {
          QLineEdit *edit = new QLineEdit;
          h.widget = edit;
          edit->setText(paramDefault);
  
        } else if ( widget_type == "Combo" ) {
          QComboBox *combo = new QComboBox;
          h.widget = combo;

          combo->setObjectName(labelName);
          int count = 0, active=0;

          QDomElement item = param.firstChildElement("Item");
          for( ; !item.isNull(); item=item.nextSiblingElement("Item") ) {
            QString itemType = item.attribute( "Type", "" );
            if ( itemType == "Active" ) active=count;
            combo->insertItem(count++,item.text().trimmed() );
          } 
          combo->setCurrentIndex(active);

        } else if ( widget_type == "CheckBox" ) {
          QCheckBox *l = new QCheckBox;
          h.widget = l;
          l->setText("");
          l->setChecked(false);
          if ( paramDefault == "true" ) l->setChecked(true);
          connect(l, SIGNAL(stateChanged(int)), this, SLOT(lSlot(int)));
        } else if ( widget_type == "Label" ) {
          QLabel *label = new QLabel;
          QFont font;
          font.setBold(true);
          font.setUnderline(true);
          label->setFont(font);
          label->setText(labelName);
          h.widget = label;
        }

        if ( h.widget ) {
          h.widget->setWhatsThis(whatis);
          h.widget->setStatusTip(statusTip);

          QString q = "/"+name.text().trimmed()+"/"+Section+"/"+labelName+"/"+QString::number(ID);
          h.widget->setProperty( "dom address",q);
          h.elem=param;
          hash[q] = h;

           h.widget->setFixedHeight(20);
          if ( widget_type != "Label" ) {
            QLabel *label = new QLabel;
            label->setText(labelName);
            grid->addWidget(label, params, 0);
            grid->addWidget(h.widget, params, 1);
          } else {
            grid->addWidget(h.widget, params, 0);
          }
        }
      }
    }
    // add a dummy frame for stretching
    QFrame *dummyFrame = new QFrame;
    grid->addWidget(dummyFrame, params, 0);
  
    // put grid in a frame
    QFrame *frm = new QFrame;
    frm->setLayout(grid);
 
   // add frame to tab
   if (params>0) {
     tabWidget->addTab(frm, name.text().trimmed());
   }

    tabs++;
    element = element.nextSiblingElement();
  }

  // Buttons:
  //----------
  QLabel *lbl = new QLabel;
  lbl->setText("Name:");

  nameEdit  = new QLineEdit;
  nameEdit->setText(Section + " " + QString::number(ID));

  applyButton = new QPushButton(tr("&Add"));
  applyButton->setIcon(addIcon);
  connect(applyButton, SIGNAL(clicked()), this, SLOT(applyButtonClicked()));
  
  discardButton = new QPushButton(tr("&Remove"));
  discardButton->setIcon(removeIcon);
  connect(discardButton, SIGNAL(clicked()), this, SLOT(discardButtonClicked()));

  QHBoxLayout *buttonLayout = new QHBoxLayout;  
  buttonLayout->addWidget(lbl);
  buttonLayout->addWidget(nameEdit);
  buttonLayout->addWidget(applyButton);
  buttonLayout->addWidget(discardButton);

  // Main layout:
  //-------------
  QVBoxLayout *mainLayout = new QVBoxLayout;
  mainLayout->addWidget(tabWidget);
  mainLayout->addLayout(buttonLayout);
  setLayout(mainLayout);

  // Window title:
  //---------------
  setWindowTitle(Section);
}


void DynamicEditor::lSlot(int state)
{
  QDomElement param;
  QString q = QObject::sender()->property("dom address").toString();

  param = hash[q].elem.firstChildElement("Activate");
  for( ;!param.isNull(); param=param.nextSiblingElement("Activate") ) {
    q = param.text().trimmed();
    hash[q].widget->setEnabled(state);
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
void DynamicEditor::applyButtonClicked()
{
  cout << "Dynamic editor: Add-button clicked" << endl;
  cout.flush();
  touched = true;
  close();
}

//----------------------------------------------------------------------------
void DynamicEditor::discardButtonClicked()
{
  cout << "Dynamic editor: Remove-button clicked" << endl;
  cout.flush();
  touched = false;
  close();
}

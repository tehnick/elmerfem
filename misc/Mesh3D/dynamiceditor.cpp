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

  menuAction = NULL;
  ID = -1;

  touched = false;
}

//----------------------------------------------------------------------------
DynamicEditor::~DynamicEditor()
{
}

//----------------------------------------------------------------------------
void DynamicEditor::setupTabs(QDomDocument &elmerDefs, QString Section, int ID)
{
  this->ID = ID;

  // Get root element of elmerDefs:
  //-------------------------------
  root = elmerDefs.documentElement();

  tabWidget = new QTabWidget;
  tabWidget->setTabShape(QTabWidget::Triangular);

  all_stuff = root.firstChildElement("ALL");
  element   = root.firstChildElement("PDE");

  tabs = 0;

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
        QString widget_enabled = param.attribute("Enabled","True");

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
          connect(edit, SIGNAL(returnPressed()), this, SLOT(editSlot()));

        } else if ( widget_type == "Combo" ) {
          QComboBox *combo = new QComboBox;
          h.widget = combo;

          combo->setObjectName(labelName);
          int count = 0, active=0;

          QDomElement item = param.firstChildElement("Item");
          for( ; !item.isNull(); item=item.nextSiblingElement("Item") ) {
            QString itemType = item.attribute( "Type", "" );
            if ( itemType == "Active" ) active=count;
            QDomElement itemName = item.firstChildElement("Name");
            combo->insertItem(count++,itemName.text().trimmed() );
          } 
          combo->setCurrentIndex(active);
          connect(combo, SIGNAL(activated(QString)), this, SLOT(comboSlot(QString)));

        } else if ( widget_type == "CheckBox" ) {
          QCheckBox *l = new QCheckBox;
          h.widget = l;
          l->setText("");
          l->setChecked(false);
          if ( paramDefault == "True" ) l->setChecked(true);
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

          if ( widget_enabled == "False" ) h.widget->setEnabled(false);

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

  QHBoxLayout *spareButtonLayout = new QHBoxLayout;  
  spareButton = new QPushButton(tr("SpareButton"));;
  spareButton->setVisible(false);
  spareButtonLayout->addWidget(spareButton);

  // Main layout:
  //-------------
  QVBoxLayout *mainLayout = new QVBoxLayout;
  mainLayout->addWidget(tabWidget);
  mainLayout->addLayout(buttonLayout);
  mainLayout->addLayout(spareButtonLayout);
  setLayout(mainLayout);

  // Window title:
  //---------------
  setWindowTitle(Section);
}

//----------------------------------------------------------------------------
void DynamicEditor::editSlot()
{
  QLineEdit *q = (QLineEdit *)QObject::sender();
  cout << string(q->text().toAscii()) << endl;
}

//----------------------------------------------------------------------------
void DynamicEditor::lSlot(int state)
{
  QDomElement param;
  QString q = QObject::sender()->property("dom address").toString();

  int ind = q.lastIndexOf( '/', -1); 
  QString ID = q.mid(ind,-1);

  param = hash[q].elem.firstChildElement("Activate");
  for( ;!param.isNull(); param=param.nextSiblingElement("Activate") ) {
    q = param.text().trimmed() + ID;
    hash[q].widget->setEnabled(state);
  }
}

//----------------------------------------------------------------------------
void DynamicEditor::comboSlot(QString select)
{
  QString q = QObject::sender()->property("dom address").toString();
  QDomElement item;

  int ind = q.lastIndexOf( '/', -1); 
  QString ID = q.mid(ind,-1);

  item = hash[q].elem.firstChildElement("Item");
  for( ;!item.isNull(); item=item.nextSiblingElement("Item") ) {
    QDomElement itemName = item.firstChildElement("Name");
    if ( itemName.text().trimmed() != select ) {
      QDomElement activ;

      activ = item.firstChildElement("Activate");
      for( ;!activ.isNull(); activ=activ.nextSiblingElement("Activate") ) {
        QString s=activ.text().trimmed() + ID;

        QString widget_enabled = hash[s].elem.attribute("Enabled","True");
        if ( widget_enabled == "False" ) 
          hash[s].widget->setEnabled(false);
        else
          hash[s].widget->setEnabled(true);
      }
    }
  }

  item = hash[q].elem.firstChildElement("Item");
  for( ;!item.isNull(); item=item.nextSiblingElement("Item") ) {
    QDomElement itemName = item.firstChildElement("Name");
    if ( itemName.text().trimmed() == select ) {
      QDomElement activ;
      activ = item.firstChildElement("Activate");
      for( ;!activ.isNull(); activ=activ.nextSiblingElement("Activate") ) {
        QString s=activ.text().trimmed() + ID;
        hash[s].widget->setEnabled(true);
      }
    }
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
#define MAT_OK     0
#define MAT_DELETE 1

  cout << "Dynamic editor: Add-button clicked" << endl;
  cout.flush();

  touched = true;

  emit(dynamicEditorReady(MAT_OK, ID));

  close();
}

//----------------------------------------------------------------------------
void DynamicEditor::discardButtonClicked()
{
#define MAT_OK     0
#define MAT_DELETE 1

  cout << "Dynamic editor: Remove-button clicked" << endl;
  cout.flush();

  touched = false;

  emit(dynamicEditorReady(MAT_DELETE, ID));

  close();
}

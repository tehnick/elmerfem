#include <iostream>
#include "egini.h"

using namespace std;

EgIni::EgIni(QWidget *parent)
  : QDialog(parent)
{
  iniLoaded = false;

  // Determine ini-file location and name:
  //--------------------------------------
  char *elmerGuiHome = NULL;
  
#ifdef __APPLE__
  QString iniFileName = this->homePath +  "/edf/egini.xml";          
#else
  QString iniFileName = "edf/egini.xml";
  elmerGuiHome = getenv("ELMERGUI_HOME");
  if(elmerGuiHome != NULL) 
    iniFileName = QString(elmerGuiHome) + "/edf/egini.xml";
#endif
  
  // Load initialization file:
  //---------------------------
  cout << "Load " << string(iniFileName.toAscii()) << "...";
  cout.flush();
  
  QFile file(iniFileName);
  QString errStr;
  int errRow;
  int errCol;

  if(!file.exists()) {

    QMessageBox::information(window(), tr("Eg ini-file loader: ") + iniFileName,
			     tr("Initialization file does not exist"));
    return;
    
  } else {  
    
    if(!iniDoc.setContent(&file, true, &errStr, &errRow, &errCol)) {

      QMessageBox::information(window(), tr("Eg ini-file loader: ") + iniFileName,
			       tr("Parse error at line %1, col %2:\n%3")
			       .arg(errRow).arg(errCol).arg(errStr));
      file.close();
      return;
      
    } else {

      if(iniDoc.documentElement().tagName() != "egini") {
	QMessageBox::information(window(), tr("Eg ini-file loader: ") + iniFileName,
				 tr("This is not an eg initialization file"));
	file.close();	
	return;
      }
    }
  }
  
  cout << " done" << endl;
  file.close();
  iniLoaded = true;
}


EgIni::~EgIni()
{
}


bool EgIni::isPresent(QString tag)
{
  if(!iniLoaded)
    return false;

  root = iniDoc.documentElement();
  element = root.firstChildElement(tag);
  
  if(element.isNull())
    return false;

  return true;
}


bool EgIni::isSet(QString tag)
{
  if(!iniLoaded)
    return false;

  root = iniDoc.documentElement();
  element = root.firstChildElement(tag);
  
  if(element.isNull())
    return false;

  if(element.text().trimmed() != "0")
    return true;
  
  return false;
}


QString EgIni::value(QString tag)
{
  if(!iniLoaded)
    return "";

  root = iniDoc.documentElement();
  element = root.firstChildElement(tag);
  
  if(element.isNull())
    return "";

  return element.text().trimmed();
}

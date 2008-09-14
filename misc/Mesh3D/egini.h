#ifndef EGINI_H
#define EGINI_H

#include <QWidget>
#include <QDomDocument>
#include <QFile>
#include <QMessageBox>
#include <QtGui>

class EgIni : public QDialog
{
  Q_OBJECT

public:
  EgIni(QWidget *parent = 0);
  ~EgIni();
  
  bool isPresent(QString tag);
  bool isSet(QString tag);
  QString value(QString tag);
  
private:
  bool iniLoaded;
  QDomDocument iniDoc;
  QDomElement root;
  QDomElement element;

};

#endif // EGINI_H

#ifndef DYNAMICEDITOR_H
#define DYNAMICEDITOR_H

#include <QWidget>
#include <QtGui>
#include <QIcon>
#include <QDomDocument>

class QTabWidget;
class QPushButton;

class DynamicEditor : public QWidget
{
  Q_OBJECT

public:
  DynamicEditor(QWidget *parent = 0);
  ~DynamicEditor();

  QSize minimumSizeHint() const;
  QSize sizeHint() const;

  void setupTabs(QDomDocument&, QString, int);

  QTabWidget *tabWidget;
  QLineEdit *nameEdit;
  int tabs;

  QPushButton *applyButton;
  QPushButton *spareButton;
  QPushButton *discardButton;

  bool touched;


signals:

private slots:
  void applyButtonClicked();
  void discardButtonClicked();
  void lSlot(int);

private:
  QIcon addIcon;
  QIcon removeIcon;

  QDomElement root;
  QDomElement all_stuff;
  QDomElement element;
  QDomElement name;
  QDomElement section;
  QDomElement param;
};

#endif // DYNAMICEDITOR_H

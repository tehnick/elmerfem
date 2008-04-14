#ifndef DYNAMICEDITOR_H
#define DYNAMICEDITOR_H

#include <QWidget>
#include <QtGui>
#include <QIcon>
#include <QDomDocument>
#include <QLayout>

class QTabWidget;
class QPushButton;

class hash_entry_t
{
 public:
  QWidget *widget,*label;
  QDomElement elem;
};

class DynLineEdit : public QWidget
{
  Q_OBJECT

public:
  DynLineEdit(QWidget *parent=0);
 ~DynLineEdit();
  QString name;
  QLineEdit *lineEdit;

private:
  QTextEdit *textEdit;
  QLabel *label;
  QFrame *frame;
  QLayout *layout;
  QPushButton *closeButton;

private slots:
  void editSlot();
  void lineEditClose();
};

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

  QGroupBox *spareBox;
  QScrollArea *spareScroll;

  QPushButton *okButton;
  QPushButton *newButton;
  QPushButton *applyButton;
  QPushButton *spareButton;
  QPushButton *discardButton;

  QAction *menuAction;  // action for menu item
  int ID;               // id in propertyarray

  bool touched;

  QHash<QString, hash_entry_t> hash;

signals:
  void dynamicEditorReady(int, int);
  void dynamicEditorSpareButtonClicked(int, int);

private slots:
  void okButtonClicked();
  void newButtonClicked();
  void applyButtonClicked();
  void discardButtonClicked();
  void spareButtonClicked();
  void lSlot(int);
  void comboSlot(QString);
  void textChangedSlot(QString);

private:
  hash_entry_t h;

  QIcon newIcon;
  QIcon addIcon;
  QIcon okIcon;
  QIcon removeIcon;

  QDomElement root;
  QDomElement all_stuff;
  QDomElement element;
  QDomElement name;
  QDomElement section;
  QDomElement param;
};

#endif // DYNAMICEDITOR_H

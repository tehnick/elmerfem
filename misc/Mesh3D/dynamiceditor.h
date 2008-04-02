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
  QWidget *widget;
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

  QPushButton *applyButton;
  QPushButton *spareButton;
  QPushButton *discardButton;

  QAction *menuAction;  // action for menu item
  int ID;               // id in propertyarray

  bool touched;

  QHash<QString, hash_entry_t> hash;

signals:
  void dynamicEditorReady(int, int);

private slots:
  void applyButtonClicked();
  void discardButtonClicked();
  void lSlot(int);
  void comboSlot(QString);

private:
  hash_entry_t h;

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

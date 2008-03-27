#ifndef DYNAMICEDITOR_H
#define DYNAMICEDITOR_H

#include <QWidget>
#include <QIcon>
#include <QDomDocument>

class QTabWidget;
class QPushButton;

// Test data structure:
//---------------------
#define EDIT_FIELD    100
#define COMBO_FIELD   101

class field_t {
 public:
  int type;
  QString label;
  QString editDefault;
  int comboEntries;
  QString *comboEntry;
};

class tab_t {
 public:
  QString name;
  int fields;
  field_t *field;
};

class DynamicEditor : public QWidget
{
  Q_OBJECT

public:
  DynamicEditor(QWidget *parent = 0);
  ~DynamicEditor();

  QSize minimumSizeHint() const;
  QSize sizeHint() const;

  int tabs;
  tab_t *tab;

  QTabWidget *tabWidget;

signals:

private slots:
  void addButtonClicked();
  void removeButtonClicked();

private:
  QDomDocument domDocument;
  QString errStr;
  int errRow;
  int errCol;

  QPushButton *addButton;
  QPushButton *removeButton;

  QIcon addIcon;
  QIcon removeIcon;

  QDomElement root;
  QDomElement element;
  QDomElement name;
  QDomElement material;
  QDomElement param;

  tab_t *t;
  field_t *f;
};

#endif // DYNAMICEDITOR_H

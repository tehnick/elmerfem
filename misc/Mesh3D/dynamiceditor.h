#ifndef DYNAMICEDITOR_H
#define DYNAMICEDITOR_H

#include <QWidget>
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

  void setupTabs(QDomDocument&, QString);

  QTabWidget *tabWidget;
  int tabs;

signals:

private slots:
  void addButtonClicked();
  void removeButtonClicked();
  void lSlot(int);

private:
  QIcon addIcon;
  QIcon removeIcon;

  QPushButton *addButton;
  QPushButton *removeButton;

  QDomElement root;
  QDomElement all_stuff;
  QDomElement element;
  QDomElement name;
  QDomElement section;
  QDomElement param;
};

#endif // DYNAMICEDITOR_H

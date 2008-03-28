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

  void setupTabs(QDomDocument&);

  QTabWidget *tabWidget;
  int tabs;

signals:

private slots:
  void addButtonClicked();
  void removeButtonClicked();
  void DynamicEditor::lSlot(int);

private:
  QIcon addIcon;
  QIcon removeIcon;

  QPushButton *addButton;
  QPushButton *removeButton;

  QDomElement root;
  QDomElement element;
  QDomElement name;
  QDomElement material;
  QDomElement param;
};

#endif // DYNAMICEDITOR_H

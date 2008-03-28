#ifndef EDFEDITOR_H
#define EDFEDITOR_H

#include <QWidget>
#include <QDomDocument>
#include <QIcon>
#include <QTreeWidget>
#include <QHash>

class QPushButton;

class EdfEditor : public QWidget
{
  Q_OBJECT

public:
  EdfEditor(QWidget *parent = 0);
  ~EdfEditor();

  QSize minimumSizeHint() const;
  QSize sizeHint() const;

  void setupEditor(QDomDocument&);

signals:

private slots:
  void addButtonClicked();
  void removeButtonClicked();
  void treeItemClicked(QTreeWidgetItem*, int);

private:
  QIcon addIcon;
  QIcon removeIcon;

  QTreeWidget *edfTree;

  QPushButton *addButton;
  QPushButton *removeButton;

  QDomElement root;
  QDomElement element;
  QDomElement name;
  QDomElement material;
  QDomElement param;

  QDomDocument *elmerDefs;

  QHash<QTreeWidgetItem*, QDomElement> domElementForItem;

  void insertTreeEntry(QDomElement element, 
		       QTreeWidgetItem *parentItem);

};

#endif // EDFEDITOR_H

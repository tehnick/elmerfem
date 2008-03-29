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

protected:
  void keyPressEvent(QKeyEvent*);
  void keyReleaseEvent(QKeyEvent*);

private slots:
  void addButtonClicked();
  void removeButtonClicked();
  void openButtonClicked();
  void saveAsButtonClicked();
  void applyButtonClicked();

  void treeItemClicked(QTreeWidgetItem*, int);
  void updateElement(QTreeWidgetItem*, int);

private:
  QIcon addIcon;
  QIcon removeIcon;
  QIcon openIcon;
  QIcon saveAsIcon;
  QIcon applyIcon;

  QTreeWidget *edfTree;
  QPushButton *addButton;
  QPushButton *removeButton;
  QPushButton *openButton;
  QPushButton *saveAsButton;
  QPushButton *applyButton;

  QDomDocument *elmerDefs;
  QHash<QTreeWidgetItem*, QDomElement> elementForItem;
  void insertItemForElement(QDomElement, QTreeWidgetItem*);

  QTreeWidgetItem *lastActive;
  bool ctrlPressed;
};

#endif // EDFEDITOR_H

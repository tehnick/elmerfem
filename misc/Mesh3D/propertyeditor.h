#ifndef PROPERTYEDITOR_H
#define PROPERTYEDITOR_H

#define MAX_BCS 100000

#include <QWidget>

class QTextEdit;
class QPushButton;
class QMenu;


class PropertyEditor : public QWidget
{
  Q_OBJECT

public:
  PropertyEditor(QWidget *parent = 0);
  ~PropertyEditor();

  QSize minimumSizeHint() const;
  QSize sizeHint() const;

  QTextEdit *textEdit;
  
  QString bcPropertyTable[MAX_BCS];

  void editProperties(int);

  bool heatEquationActive;
  bool linearElasticityActive;

private slots:
  void propertiesChanged();
  void clearProperties();

private:
  QPushButton *closeButton;
  QPushButton *clearButton;

  int bcIndex;

};

#endif // PROPERTYEDITOR_H

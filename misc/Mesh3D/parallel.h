#ifndef PARALLEL_H
#define PARALLEL_H

#include <QWidget>
#include "ui_parallel.h"

class Parallel : public QDialog
{
  Q_OBJECT

public:
  Parallel(QWidget *parent = 0);
  ~Parallel();

  Ui::parallelDialog ui;

signals:

private slots:
  void okButtonClicked();
  void browseButtonClicked(); 

private:
 
};

#endif // PARALLEL_H

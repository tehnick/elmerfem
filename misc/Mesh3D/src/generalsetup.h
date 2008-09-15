#ifndef GENERALSETUP_H
#define GENERALSETUP_H

#include <QWidget>
#include "ui_generalsetup.h"

class GeneralSetup : public QDialog
{
  Q_OBJECT

public:
  GeneralSetup(QWidget *parent = 0);
  ~GeneralSetup();

  Ui::setupDialog ui;

signals:

private slots:
  void acceptButtonClicked();

private:
};

#endif // GENERALSETUP_H

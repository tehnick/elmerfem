#include "tester.h"

Tester::Tester(QWidget *parent)
  : QWidget(parent)
{
  ui.setupUi(this);

  connect(ui.closeButton, SIGNAL(clicked()), this, SLOT(close()));

  setWindowTitle("ElmerGUI installation tester");
  setWindowIcon(QIcon(":/img/Mesh3D.png"));

  elmerHome = get("ELMER_HOME");
  elmerGuiHome = get("ELMERGUI_HOME");
  ok = true;
}

QString Tester::get(const QString &variable)
{
  QString value(getenv(qPrintable(variable)));

#ifdef Q_OS_WIN32
  while(value.endsWith("\\"))
    value.chop(1);
#else
  while(value.endsWith("/"))
    value.chop(1);
#endif

  return value;
}

bool Tester::testDir(const QString &variable, QLabel *label)
{
  QString value(get(variable));

  label->setText(value);
  label->setAutoFillBackground(true);
  label->setPalette(QPalette(Qt::red));

  if(!value.isEmpty() && QDir(value).exists()) {
    label->setPalette(QPalette(Qt::green));
    return true;
  }

  return false;
}

bool Tester::testFile(const QString &value, QLabel *label)
{
  label->setText(value);
  label->setAutoFillBackground(true);
  label->setPalette(QPalette(Qt::red));

  if(!value.isEmpty() && QFile(value).exists()) {
    label->setPalette(QPalette(Qt::green));
    return true;
  }

  return false;
}

bool Tester::testPath(const QString &value, QLabel *label)
{
  QString path(get("PATH"));

#ifdef Q_OS_WIN32
  QStringList splitPath(path.toUpper().split(";"));
#else
  QStringList splitPath(path.split(":"));
#endif

  label->setText(value);
  label->setAutoFillBackground(true);
  label->setPalette(QPalette(Qt::red));  

#ifdef Q_OS_WIN32
  if(splitPath.contains(value.toUpper())) {
    label->setPalette(QPalette(Qt::green));
    return true;
  }
#else
  if(splitPath.contains(value)) {
    label->setPalette(QPalette(Qt::green));
    return true;
  }
#endif
  
  return false;
}

bool Tester::testLdLibraryPath(const QString &value, QLabel *label)
{
  QString ldLibraryPath(get("LD_LIBRARY_PATH"));

#ifdef Q_OS_WIN32
  QStringList splitLdLibraryPath(ldLibraryPath.toUpper().split(";"));
#else
  QStringList splitLdLibraryPath(ldLibraryPath.split(":"));
#endif

  label->setText(value);
  label->setAutoFillBackground(true);
  label->setPalette(QPalette(Qt::red));  

#ifdef Q_OS_WIN32
  if(splitLdLibraryPath.contains(value.toUpper())) {
    label->setPalette(QPalette(Qt::green));
    return true;
  }
#else
  if(splitLdLibraryPath.contains(value)) {
    label->setPalette(QPalette(Qt::green));
    return true;
  }
#endif
  
  return false;
}

void Tester::testEnvironment()
{
  ok &= testDir("ELMER_HOME", ui.elmerHomeResult);
  ok &= testDir("ELMERGUI_HOME", ui.elmerGuiHomeResult);
  ok &= testDir("ELMER_POST_HOME", ui.elmerPostHomeResult);

#ifdef Q_OS_WIN32
  ui.ldLibraryPathLabel->setText("PATH");
  ok &= testPath(elmerHome + "\\bin", ui.pathResult);
  ok &= testPath(elmerHome + "\\lib", ui.ldLibraryPathResult);
#else
  ok &= testPath(elmerHome + "/bin", ui.pathResult);
  ok &= testLdLibraryPath(elmerHome + "/lib", ui.ldLibraryPathResult);
#endif
}

void Tester::testExecutables()
{
#ifdef Q_OS_WIN32
  ok &= testFile(elmerHome + "\\bin\\ElmerSolver.exe", ui.elmerSolverResult);
  ok &= testFile(elmerGuiHome + "\\ElmerGUI.exe", ui.elmerGuiResult);
  ok &= testFile(elmerHome + "\\bin\\ElmerPost.exe", ui.elmerPostResult);
  ok &= testFile(elmerHome + "\\bin\\ElmerGrid.exe", ui.elmerGridResult);
#else
  ok &= testFile(elmerHome + "/bin/ElmerSolver", ui.elmerSolverResult);
  ok &= testFile(elmerGuiHome + "/ElmerGUI", ui.elmerGuiResult);
  ok &= testFile(elmerHome + "/bin/ElmerPost", ui.elmerPostResult);
  ok &= testFile(elmerHome + "/bin/ElmerGrid", ui.elmerGridResult);
#endif
}

void Tester::verdict()
{
  QTextEdit *e = ui.verdict;

  if(ok) {
    e->append("Elmer seems to be installed correctly on this system");
    return;
  }

  e->append("Elmer seems to be installed incorrectly on this system");
  e->append("1) Make sure that ELMER_HOME has been set up properly");
  e->append("2) Set ELMERGUI_HOME to ELMER_HOME/bin");
  e->append("3) Set ELMER_POST_HOME to ELMER_HOME/share/elmerpost");
  e->append("4) Make sure that ELMER_HOME/bin is in PATH");
#ifdef Q_OS_WIN32
  e->append("5) Make sure that ELMER_HOME/lib is in PATH");
#else
  e->append("5) Make sure that ELMER_HOME/lib is in LD_LIBRARY_PATH");
#endif
  e->append("6) Executables should be found from ELMER_HOME/bin");
}

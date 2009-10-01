#include "tester.h"

Tester::Tester(QWidget *parent)
  : QWidget(parent)
{
  ui.setupUi(this);

  connect(ui.closeButton, SIGNAL(clicked()), this, SLOT(close()));

  setWindowTitle("ElmerGUI installation tester");

  kosher = true;
}

void Tester::testEnvironment()
{
  QPalette green(Qt::green);
  QPalette red(Qt::red);
  
  QString elmerHome(getenv("ELMER_HOME"));
  ui.elmerHomeResult->setText(elmerHome);
  ui.elmerHomeResult->setAutoFillBackground(true);
  ui.elmerHomeResult->setPalette(red);
  if(!elmerHome.isEmpty() && QDir(elmerHome).exists())
    ui.elmerHomeResult->setPalette(green);
  else
    kosher = false;

  QString elmerGuiHome(getenv("ELMERGUI_HOME"));
  ui.elmerGuiHomeResult->setText(elmerGuiHome);
  ui.elmerGuiHomeResult->setAutoFillBackground(true);
  ui.elmerGuiHomeResult->setPalette(red);    
  if(!elmerGuiHome.isEmpty() && QDir(elmerGuiHome).exists())
    ui.elmerGuiHomeResult->setPalette(green);
  else
    kosher = false;

  QString elmerPostHome(getenv("ELMER_POST_HOME"));
  ui.elmerPostHomeResult->setText(elmerPostHome);
  ui.elmerPostHomeResult->setAutoFillBackground(true);
  ui.elmerPostHomeResult->setPalette(red);    
  if(!elmerPostHome.isEmpty() && QDir(elmerPostHome).exists())
    ui.elmerPostHomeResult->setPalette(green);
  else
    kosher = false;

  QString path(getenv("PATH"));
#ifdef Q_OS_WIN32
  QString targetPath(elmerHome + "\\bin");
  ui.pathResult->setText(targetPath);
  ui.pathResult->setAutoFillBackground(true);
  ui.pathResult->setPalette(red);  

  QStringList pathList = path.split(";");
  if(pathList.contains(targetPath))
    ui.pathResult->setPalette(green);  
  else
    kosher = false;

  ui.ldLibraryPathLabel->setText("PATH");
  targetPath = elmerHome + "\\lib";
  ui.ldLibraryPathResult->setText(targetPath);
  ui.ldLibraryPathResult->setAutoFillBackground(true);
  ui.ldLibraryPathResult->setPalette(red);  

  pathList = path.split(";");
  if(pathList.contains(targetPath))
    ui.ldLibraryPathResult->setPalette(green);
  else
    kosher = false;
#else
#endif
}

void Tester::testExecutables()
{
  QPalette green(Qt::green);
  QPalette red(Qt::red);
  
  QString elmerHome(getenv("ELMER_HOME"));
#ifdef Q_OS_WIN32
  QString elmerSolver(elmerHome + "\\bin\\ElmerSolver.exe");
#else
  QString elmerSolver(elmerHome + "/bin/ElmerSolver");
#endif
  ui.elmerSolverResult->setText(elmerSolver);
  ui.elmerSolverResult->setAutoFillBackground(true);
  ui.elmerSolverResult->setPalette(red);
  if(QFile(elmerHome).exists())
    ui.elmerSolverResult->setPalette(green);
  else
    kosher = false;

  QString elmerGuiHome(getenv("ELMERGUI_HOME"));
#ifdef Q_OS_WIN32
  QString elmerGui(elmerGuiHome + "\\ElmerGUI.exe");
#else
  QString elmerGui(elmerGuiHome + "/ElmerGUI");
#endif
  ui.elmerGuiResult->setText(elmerGui);
  ui.elmerGuiResult->setAutoFillBackground(true);
  ui.elmerGuiResult->setPalette(red);
  if(QFile(elmerGui).exists())
    ui.elmerGuiResult->setPalette(green);
  else
    kosher = false;

#ifdef Q_OS_WIN32
  QString elmerPost(elmerHome + "\\bin\\ElmerPost.exe");
#else
  QString elmerPost(elmerHome + "/bin/ElmerPost");
#endif
  ui.elmerPostResult->setText(elmerPost);
  ui.elmerPostResult->setAutoFillBackground(true);
  ui.elmerPostResult->setPalette(red);
  if(QFile(elmerPost).exists())
    ui.elmerPostResult->setPalette(green);
  else
    kosher = false;

#ifdef Q_OS_WIN32
  QString elmerGrid(elmerHome + "\\bin\\ElmerGrid.exe");
#else
  QString elmerGrid(elmerHome + "/bin/ElmerGrid");
#endif
  ui.elmerGridResult->setText(elmerGrid);
  ui.elmerGridResult->setAutoFillBackground(true);
  ui.elmerGridResult->setPalette(red);
  if(QFile(elmerGrid).exists())
    ui.elmerGridResult->setPalette(green);
  else
    kosher = false;
}

void Tester::verdict()
{
  QTextEdit *e = ui.verdict;

  if(kosher) {
    e->append("Elmer seems to be installed correctly on this system");
    return;
  }

  e->append("Elmer seems to be installed incorrectly on this system");
  e->append("1) Make sure that ELMER_HOME has been set up properly");
  e->append("2) Set ELMERGUI_HOME to ELMER_HOME/bin");
  e->append("3) Set ELMER_POST_HOME to ELMER_HOME/share/elmerpost");
#ifdef Q_OS_WIN32
  e->append("4) Make sure that ELMER_HOME/bin is in PATH");
  e->append("5) Make sure that ELMER_HOME/lib is in PATH");
  e->append("6) Executables should be found from ELMER_HOME/bin");
#else
#endif
}

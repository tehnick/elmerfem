#ifndef GLWIDGET_H
#define GLWIDGET_H

#define EDGELIST        2000
#define SURFACELIST     2001
#define SURFACEEDGELIST 2002

#include <QGLWidget>
#include "helpers.h"
#include "meshutils.h"

class list_t {
 public:
  int nature;        // PDE_BOUNDARY, PDE_BULK, ...
  int type;          // EDGELIST, SURFACELIST, ...
  int index;         // Boundary condition as defined in input file
  GLuint object;     // GL list index as returned by glGenLists()
  bool selected;     // Currently selected?
  bool visible;      // Currently visible?
};

class GLWidget : public QGLWidget
{
    Q_OBJECT

public:
    GLWidget(QWidget *parent = 0);
    ~GLWidget();

    QSize minimumSizeHint() const;
    QSize sizeHint() const;

    mesh_t *mesh;

    int lists;
    list_t *list;
    GLuint makeLists();
    void rebuildLists();

    double drawTranslate[3];
    double drawScale;

public slots:

signals:
 void signalBoundarySelected(list_t*);

protected:
    void initializeGL();
    void paintGL();
    void resizeGL(int, int);

    void mouseDoubleClickEvent(QMouseEvent*);
    void mousePressEvent(QMouseEvent*);
    void mouseMoveEvent(QMouseEvent*);
    void wheelEvent(QWheelEvent*);
    void keyPressEvent(QKeyEvent*);
    void keyReleaseEvent(QKeyEvent*);

private:
    Helpers *helpers;
    Meshutils *meshutils;

    GLdouble matrix[16];
    GLdouble invmatrix[16];
    void getMatrix();

    QPoint lastPos;
    QColor backgroundColor;

    GLuint generateSurfaceList(int, double, double, double);
    GLuint generateSurfaceEdgeList(int, double, double, double);
    GLuint generateEdgeList(int, double, double, double);
    
    bool ctrlPressed; // true while ctrl key is held down
};

#endif

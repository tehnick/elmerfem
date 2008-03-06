#ifndef GLWIDGET_H
#define GLWIDGET_H

#include <QGLWidget>
#include "helpers.h"
#include "meshutils.h"

class GLWidget : public QGLWidget
{
    Q_OBJECT

public:
    GLWidget(QWidget *parent = 0);
    ~GLWidget();

    QSize minimumSizeHint() const;
    QSize sizeHint() const;

    Helpers helpers;
    Meshutils meshutils;
    mesh_t *mesh;

    GLuint objects;
    GLuint makeObjects();

    double drawScale;
    double drawTranslate[3];

    int sizeofGlMaps;
    int *glBcMap;         // maps {0,1,2,..} -> {bc indices}
    GLuint *glListMap;    // maps {0,1,2,..} -> {gl list indices]
    bool *glSelected;     // indicates currently selected bcs

public slots:

signals:
    void signalBoundarySelected(int);

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

    GLdouble matrix[16];
    GLdouble invmatrix[16];
    void getMatrix();

    QPoint lastPos;
    QColor backgroundColor;

    GLuint generateBoundaryList(int,double,double,double);
    
    bool ctrlPressed;
};

#endif

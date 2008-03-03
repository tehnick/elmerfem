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
    GLuint firstList;
    GLuint lastList;
    GLuint makeObjects();

    unsigned char *colorMap;
    GLint colorMapEntries;

    double drawScale;
    double drawTranslate[3];

public slots:

signals:
    void signalBoundarySelected(int);

protected:
    void initializeGL();
    void paintGL();
    void resizeGL(int, int);

    void clearColorMap();

    void mouseDoubleClickEvent(QMouseEvent*);
    void mousePressEvent(QMouseEvent*);
    void mouseMoveEvent(QMouseEvent*);
    void wheelEvent(QWheelEvent*);

private:

    GLdouble matrix[16];
    GLdouble invmatrix[16];
    void getMatrix();

    QPoint lastPos;
    QColor backgroundColor;
};

#endif

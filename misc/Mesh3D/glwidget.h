#ifndef GLWIDGET_H
#define GLWIDGET_H

#include <QGLWidget>
#include "helpers.h"

class GLWidget : public QGLWidget
{
    Q_OBJECT

public:
    GLWidget(QWidget *parent = 0);
    ~GLWidget();

    QSize minimumSizeHint() const;
    QSize sizeHint() const;

    Helpers helpers;
    mesh_t *mesh;

    GLuint objects;
    GLuint firstList;
    GLuint lastList;
    GLuint makeObjects();

    unsigned char *colorMap;
    GLint colorMapEntries;

    double drawScale;
    double drawTranslate[3];

    void clearMesh();
    void findBoundaryElementEdges(mesh_t*);

public slots:

signals:
    void selectedBoundary(int boundary);

protected:
    void initializeGL();
    void paintGL();
    void resizeGL(int width, int height);

    void clearColorMap();

    void mouseDoubleClickEvent(QMouseEvent *event);
    void mousePressEvent(QMouseEvent *event);
    void mouseMoveEvent(QMouseEvent *event);
    void wheelEvent(QWheelEvent *event);

private:

    GLdouble matrix[16];
    GLdouble invmatrix[16];
    void getMatrix();

    QPoint lastPos;
    QColor backgroundColor;
};

#endif

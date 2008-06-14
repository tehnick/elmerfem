/****************************************************************************
**
** This file is part of the QtOpenCascade Toolkit.
**
** This file may be used under the terms of the GNU General Public
** License version 2.0 as published by the Free Software Foundation
** and appearing in the file LICENSE.GPL included in the packaging of
** this file.
**
** Copyright (C) Peter Dolbey 2006-7. All rights reserved.
**
** This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
** WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
**
****************************************************************************/
#ifndef QOCCVIEWERCONTEXT_H
#define QOCCVIEWERCONTEXT_H

#include <QtCore/QObject>
#include "qocc.h"

class QOCC_DECLSPEC QoccViewerContext : public QObject
{

	Q_OBJECT

public:

    QoccViewerContext();
    ~QoccViewerContext();

	Handle_V3d_Viewer&              getViewer();
	Handle_AIS_InteractiveContext&  getContext();

	Handle_V3d_Viewer createViewer(	const Standard_CString aDisplay,
									const Standard_ExtString aName,
									const Standard_CString aDomain,
									const Standard_Real ViewSize );

	void deleteAllObjects();

	void setGridOffset (Quantity_Length offset);

public slots:

	void gridXY   ( void );
	void gridXZ   ( void );
	void gridYZ   ( void );
	void gridOn   ( void );
	void gridOff  ( void );
	void gridRect ( void );
	void gridCirc ( void );

signals:

	void error (int errorCode, QString& errorDescription);

private:

	Handle_V3d_Viewer				myViewer;
	Handle_AIS_InteractiveContext	myContext;
	Aspect_GridType					myGridType;
	Aspect_GridDrawMode				myGridMode;
	Quantity_NameOfColor			myGridColor;
	Quantity_NameOfColor			myGridTenthColor;

};

#endif // QOCCVIEWERCONTEXT_H

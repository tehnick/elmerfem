/****************************************************************************
**
** This file is part of the QtOpenCascade Toolkit.
**
** This file may be used under the terms of the GNU General Public
** License version 2.0 as published by the Free Software Foundation
** and appearing in the file LICENSE.GPL included in the packaging of
** this file.
**
** This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
** WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
**
** Copyright (C) 2006-7 Peter Dolbey. All rights reserved.
**
****************************************************************************/
/*!
\class	QoccViewerContext 
\brief	This class provides a simple document element of the QtOpenCascade Toolkit.
	
		This class provides a interactive QObject derived container for both
		the AIS context and its View manager, providing a very simple "document"
		model,

\author	Peter C. Dolbey
*/
#include <qnamespace.h>
#include "qoccviewercontext.h"
#include "qoccinternal.h"

QoccViewerContext::QoccViewerContext()
{
	// Create the OCC Viewers
	TCollection_ExtendedString a3DName("Visual3D");
	myViewer = createViewer( "DISPLAY", a3DName.ToExtString(), "", 1000.0 );
	myViewer->Init();
	myViewer->SetZBufferManagment(Standard_False);
	myViewer->SetDefaultViewProj( V3d_Zpos );	// Top view
 	myContext = new AIS_InteractiveContext( myViewer );

	myGridType       = Aspect_GT_Rectangular;
	myGridMode       = Aspect_GDM_Lines;
	myGridColor      = Quantity_NOC_RED4;
	myGridTenthColor = Quantity_NOC_GRAY90;

	// myContext->SetHilightColor(Quantity_NOC_WHITE) ;
	myContext->SetHilightColor(Quantity_NOC_BLACK) ;

	setGridOffset (0.0);
	gridXY();
	gridOn();
}


QoccViewerContext::~QoccViewerContext()
{

}

Handle_V3d_Viewer& QoccViewerContext::getViewer()  
{ 
	return myViewer; 
}

Handle_AIS_InteractiveContext& QoccViewerContext::getContext() 
{ 
	return myContext; 
}

Handle_V3d_Viewer QoccViewerContext::createViewer(	const Standard_CString aDisplay,
													const Standard_ExtString aName,
													const Standard_CString aDomain,
													const Standard_Real ViewSize )
{
#ifndef WNT
    static Handle(Graphic3d_GraphicDevice) defaultdevice;
	
    if( defaultdevice.IsNull() )
	{
		defaultdevice = new Graphic3d_GraphicDevice( getenv(aDisplay) );
	}

    return new V3d_Viewer(	defaultdevice,
							aName,
							aDomain,
							ViewSize,
							V3d_XposYnegZpos,
							Quantity_NOC_BLACK,
                    		V3d_ZBUFFER,
							V3d_GOURAUD,
							V3d_WAIT );
#else
    static Handle( Graphic3d_WNTGraphicDevice ) defaultdevice;
    if( defaultdevice.IsNull() )
	{
        defaultdevice = new Graphic3d_WNTGraphicDevice();
	}

    return new V3d_Viewer(	defaultdevice,
							aName,
							aDomain,
							ViewSize,
							V3d_XposYnegZpos,
							Quantity_NOC_BLACK,
                    		V3d_ZBUFFER,
							V3d_GOURAUD,
							V3d_WAIT );
#endif  // WNT
}
/*! 
\brief	Deletes all objects.

		This function deletes all dispayed objects from the AIS context.
		No parameters.
*/
void QoccViewerContext::deleteAllObjects()
{
	AIS_ListOfInteractive aList;
	myContext->DisplayedObjects( aList );
	AIS_ListIteratorOfListOfInteractive aListIterator;
	for ( aListIterator.Initialize( aList ); aListIterator.More(); aListIterator.Next() )
	{
		myContext->Remove( aListIterator.Value(), Standard_False);
	}
}
/*! 
\brief	Sets the privileged plane to the XY Axis.  
*/
void QoccViewerContext::gridXY  ( void )
{
	myGridColor      = Quantity_NOC_RED4;
	myViewer->Grid()->SetColors( myGridColor, myGridTenthColor );
	gp_Ax3 aPlane(gp_Pnt( 0., 0., 0. ),gp_Dir(0., 0., 1.));
	myViewer->SetPrivilegedPlane( aPlane );
}
/*!
\brief	Sets the privileged plane to the XZ Axis.

		Note the negative direction of the Y axis.
		This is corrrect for a right-handed co-ordinate set.
*/
void QoccViewerContext::gridXZ  ( void )
{
	myGridColor      = Quantity_NOC_BLUE4;
	myViewer->Grid()->SetColors( myGridColor, myGridTenthColor );
	gp_Ax3 aPlane( gp_Pnt(0., 0., 0.),gp_Dir(0., -1., 0.) );
	myViewer->SetPrivilegedPlane( aPlane );
}
/*! 
\brief	Sets the privileged plane to the XY Axis.
 */
void QoccViewerContext::gridYZ  ( void )
{
	myGridColor      = Quantity_NOC_GREEN4;
	myViewer->Grid()->SetColors( myGridColor, myGridTenthColor );
	gp_Ax3 aPlane( gp_Pnt( 0., 0., 0.), gp_Dir( 1., 0., 0. ) );
	myViewer->SetPrivilegedPlane( aPlane );
}

/*!
\brief	Turn the grid on.
 */
void QoccViewerContext::gridOn  ( void )
{
	myViewer->ActivateGrid( myGridType , myGridMode );
	myViewer->SetGridEcho ( Standard_True );
}

/*! 
\brief	Turn the grid off.
*/
void QoccViewerContext::gridOff ( void )
{
	myViewer->DeactivateGrid();
	myViewer->SetGridEcho( Standard_False );
}

/*!
\brief	Select a rectangular grid
*/
void QoccViewerContext::gridRect ( void )
{
	myGridType = Aspect_GT_Rectangular;
	myViewer->ActivateGrid( myGridType , myGridMode );
	myViewer->Grid()->SetColors( myGridColor, myGridTenthColor );
}
/*! 
\brief	Select a circular grid.
*/
void QoccViewerContext::gridCirc ( void )
{
	myGridType = Aspect_GT_Circular;
	myViewer->ActivateGrid( myGridType , myGridMode );
	myViewer->Grid()->SetColors( myGridColor, myGridTenthColor );
}

void QoccViewerContext::setGridOffset (Quantity_Length offset)
{
	Quantity_Length radius;
	Quantity_Length xSize, ySize;
	Quantity_Length oldOffset;
	
	myViewer->CircularGridGraphicValues( radius, oldOffset );
	myViewer->SetCircularGridGraphicValues( radius, offset);

	myViewer->RectangularGridGraphicValues(xSize, ySize, oldOffset);
	myViewer->SetRectangularGridGraphicValues(xSize, ySize, offset);
}



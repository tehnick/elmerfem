/*****************************************************************************
 *                                                                           *
 *  Elmer, A Finite Element Software for Multiphysical Problems              *
 *                                                                           *
 *  Copyright 1st April 1995 - , CSC - IT Center for Science Ltd., Finland   *
 *                                                                           *
 *  This program is free software; you can redistribute it and/or            *
 *  modify it under the terms of the GNU General Public License              *
 *  as published by the Free Software Foundation; either version 2           *
 *  of the License, or (at your option) any later version.                   *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *
 *  GNU General Public License for more details.                             *
 *                                                                           *
 *  You should have received a copy of the GNU General Public License        *
 *  along with this program (in file fem/GPL-2); if not, write to the        *
 *  Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,         *
 *  Boston, MA 02110-1301, USA.                                              *
 *                                                                           *
 *****************************************************************************/

/*****************************************************************************
 *                                                                           *
 *  ElmerClips                                                               *
 *                                                                           *
 *****************************************************************************
 *                                                                           *
 *  Authors: Mikko Lyly                                                      *
 *  Email:   Juha.Ruokolainen@csc.fi                                         *
 *  Web:     http://www.csc.fi/elmer                                         *
 *  Address: CSC - IT Center for Science Ltd.                                *
 *           Keilaranta 14                                                   *
 *           02101 Espoo, Finland                                            *
 *                                                                           *
 *  Original Date: 14 Nov 2010                                               *
 *                                                                           *
 *****************************************************************************/
#include "preview.h"

Preview::Preview(QWidget *parent) : QLabel(parent)
{
  setAlignment(Qt::AlignCenter);

  setAcceptDrops(true);

  showInfo();

  connect(&encoder, SIGNAL(drawThumbnail(const QString &)),
	  this, SLOT(drawThumbnail(const QString &)),
	  Qt::BlockingQueuedConnection);
}

void Preview::checkCommandLine()
{
  if(qApp->arguments().count() > 1) {
    QList<QUrl> urls;

    foreach(const QString &arg, qApp->arguments())
      urls << QUrl(arg);

    setAcceptDrops(false);
    encoder.setUrls(urls);
    encoder.start();
  }
}

void Preview::dragEnterEvent(QDragEnterEvent *event)
{
  if(event->mimeData()->hasFormat("text/uri-list"))
    event->acceptProposedAction();
}

void Preview::dropEvent(QDropEvent *event)
{
  if(!encoder.isRunning()) {
    setAcceptDrops(false);
    encoder.setUrls(event->mimeData()->urls());
    encoder.start();
  }  

  event->acceptProposedAction();
}

void Preview::closeEvent(QCloseEvent *event)
{
  Q_UNUSED(event);

  qApp->quit();
}

void Preview::drawThumbnail(const QString &fileName)
{
  if(fileName.startsWith("FILE")) {
    setWindowTitle(fileName);
    return;
  }

  if(fileName.startsWith("DONE")) {
    showInfo();
    setAcceptDrops(true);

    if(qApp->arguments().count() > 1)
      qApp->quit();

    return;
  }

  QPixmap pixmap(fileName);

  setPixmap(pixmap.scaledToWidth(width(), Qt::SmoothTransformation));
}

void Preview::showInfo()
{
  clear();

  setWindowTitle("ElmerClips");

  setText("Drag and drop image files and/or folders here\n\nSupported formats: png, jpg (jpeg), tiff, gif\nAutomatic ordering: first integer in file name");
}

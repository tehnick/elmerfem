; example1.nsi
;
; This script is perhaps one of the simplest NSIs you can make. All of the
; optional settings are left to their default settings. The instalelr simply 
; prompts the user asking them where to install, and drops of notepad.exe
; there. If your Windows directory is not C:\windows, change it below.
;

; The name of the installer
Name "Elmer 5.0"

; The file to write
OutFile "Elmer.exe"

; Icon c:\source\xsmiles\bin\xsmiles3.ico

; The default installation directory
InstallDir c:\elmer

; The text to prompt the user to enter a directory
DirText "This will install the Elmer on your computer. Choose a directory. Hit next to begin installation."

; The stuff to install
Section "Main"
  ; Set output path to the installation directory.
  SetOutPath $INSTDIR
  ; Put file there
  File /r "c:\cygwin\home\admin\elmerdist\*"
  SetOutPath "$INSTDIR\bin"
  ;  CreateShortCut "$DESKTOP\ElmerPost.lnk" "$INSTDIR\bin\elmerpost.bat" "" "$INSTDIR\bin\elmerpost.ico" 0
  SetOutPath "$INSTDIR"
  ; Write the installation path into the registry
  WriteRegStr HKLM SOFTWARE\Elmer5x "Install_Dir" "$INSTDIR"
  ; Write the uninstall keys for Windows
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Elmer5x" "DisplayName" "Elmer (remove only)"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Elmer5x" "UninstallString" '"$INSTDIR\uninstall.exe"'
  
  WriteUninstaller "$INSTDIR\Uninstall.exe"

SectionEnd ; end the section

; uninstall stuff

; special uninstall section.
Section "Uninstall"
  ; remove registry keys
  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Elmer5x"
  DeleteRegKey HKLM SOFTWARE\Elmer5x
  ; remove files
  ; MUST REMOVE UNINSTALLER, too
  Delete $INSTDIR\uninstall.exe
  RMDir /r "$INSTDIR"
  ; remove shortcuts, if any.
  ; remove directories used.
  Delete "$DESKTOP\ElmerPost.lnk"
  Delete "$DESKTOP\ElmerPost.pif"
SectionEnd

; eof

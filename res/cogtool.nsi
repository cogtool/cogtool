Name "CogTool"
# Defines
!define Name CogTool
!define REGKEY "SOFTWARE\$(^Name)"
!define VERSION VERSTRING
!define COMPANY "Carnegie Mellon University, HCI Institute"
!define URL http://www.cogtool.org/

# MUI defines
!define MUI_ICON "${NSISDIR}\Contrib\Graphics\Icons\orange-install.ico"
!define MUI_FINISHPAGE_NOAUTOCLOSE
!define MUI_STARTMENUPAGE_REGISTRY_ROOT HKLM
!define MUI_STARTMENUPAGE_NODISABLE
!define MUI_STARTMENUPAGE_REGISTRY_KEY Software\CogTool
!define MUI_STARTMENUPAGE_REGISTRY_VALUENAME StartMenuGroup
!define MUI_STARTMENUPAGE_DEFAULT_FOLDER CogTool
!define MUI_FINISHPAGE_RUN $INSTDIR\CogTool.exe
!define MUI_UNICON "${NSISDIR}\Contrib\Graphics\Icons\orange-uninstall.ico"

# Included files
!include Sections.nsh
!include MUI.nsh

# Reserved Files

# Variables
Var StartMenuGroup

# Installer pages
!insertmacro MUI_PAGE_WELCOME
!insertmacro MUI_PAGE_LICENSE license.txt
!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_STARTMENU Application $StartMenuGroup
!insertmacro MUI_PAGE_INSTFILES
!insertmacro MUI_PAGE_FINISH
!insertmacro MUI_UNPAGE_CONFIRM
!insertmacro MUI_UNPAGE_INSTFILES

# Installer languages
!insertmacro MUI_LANGUAGE English

# Installer attributes
OutFile ..\dist\CogTool-1_3trunk-setup.exe
InstallDir $PROGRAMFILES\CogTool
CRCCheck on
XPStyle on
ShowInstDetails hide
VIProductVersion EXEVERS
VIAddVersionKey ProductName "${Name} ${Version}"
VIAddVersionKey ProductVersion "${VERSION}"
VIAddVersionKey CompanyName "${COMPANY}"
VIAddVersionKey CompanyWebsite "${URL}"
VIAddVersionKey FileVersion "VERSTRING"
VIAddVersionKey FileDescription "${Name} ${Version} Setup"
VIAddVersionKey LegalCopyright "(c) 2012 Carnegie Mellon University, HCI Institute"
InstallDirRegKey HKLM "${REGKEY}" Path
SilentUnInstall normal

# Installer sections
Section -Main SEC0000
    SetOutPath $INSTDIR
    SetOverwrite on
    File ..\dist\CogTool.exe
    SetOutPath $INSTDIR\clisp-win
    File ..\clisp-win\*
    SetOutPath $INSTDIR\xul-win
    File /r /x .svn /x ._* ..\xul-win\*
    SetOutPath $INSTDIR\lib
    File ..\lib\*.jar
    File "..\lib\windows\*"
    SetOutPath $INSTDIR\jre
    File /r /x .svn /x ._* ..\jre\*
    
    ; Register File Type Association
    ; back up old value of .opt
    !define Index "Line${__LINE__}"
      ReadRegStr $1 HKCR ".cgt" ""
      StrCmp $1 "" "${Index}-NoBackup"
        StrCmp $1 "CogTool.Project" "${Index}-NoBackup"
        WriteRegStr HKCR ".cgt" "backup_val" $1
    "${Index}-NoBackup:"
      WriteRegStr HKCR ".cgt" "" "$(^Name).Project"
      ReadRegStr $0 HKCR "$(^Name).Project" ""
      StrCmp $0 "" 0 "${Index}-Skip"
        WriteRegStr HKCR "$(^Name).Project" "" "$(^Name) Project File"
        WriteRegStr HKCR "$(^Name).Project\shell" "" "open"
; If no default icon is defined, Windows will auto-generate from a (decent) template        
;        WriteRegStr HKCR "$(^Name).Project\DefaultIcon" "" "$INSTDIR\$(^Name).exe,0"
    "${Index}-Skip:"
      WriteRegStr HKCR "$(^Name).Project\shell\open\command" "" \
        '$INSTDIR\$(^Name).exe "%1"'
      WriteRegStr HKCR "$(^Name).Project\shell\edit" "" "Edit $(^Name) Project"
      WriteRegStr HKCR "$(^Name).Project\shell\edit\command" "" \
        '$INSTDIR\$(^Name).exe "%1"'
    !undef Index
    ; End of Registering File Type Association
    
    WriteRegStr HKLM "${REGKEY}\Components" Main 1
SectionEnd

Section -post SEC0001
    WriteRegStr HKLM "${REGKEY}" Path $INSTDIR
    WriteUninstaller $INSTDIR\CogTool-uninstall.exe
    !insertmacro MUI_STARTMENU_WRITE_BEGIN Application
    SetOutPath $SMPROGRAMS\$StartMenuGroup
    CreateShortCut "$SMPROGRAMS\$StartMenuGroup\$(^Name) ${Version}.lnk" $INSTDIR\CogTool.exe
    CreateShortCut "$SMPROGRAMS\$StartMenuGroup\Uninstall $(^Name) ${Version}.lnk" $INSTDIR\CogTool-uninstall.exe
    !insertmacro MUI_STARTMENU_WRITE_END
    WriteRegStr HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\$(^Name)" DisplayName "$(^Name)"
    WriteRegStr HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\$(^Name)" DisplayVersion "${VERSION}"
    WriteRegStr HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\$(^Name)" Publisher "${COMPANY}"
    WriteRegStr HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\$(^Name)" URLInfoAbout "${URL}"
    WriteRegStr HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\$(^Name)" DisplayIcon $INSTDIR\CogTool-uninstall.exe
    WriteRegStr HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\$(^Name)" UninstallString $INSTDIR\CogTool-uninstall.exe
    WriteRegDWORD HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\$(^Name)" NoModify 1
    WriteRegDWORD HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\$(^Name)" NoRepair 1
SectionEnd

# Macro for selecting uninstaller sections
!macro SELECT_UNSECTION SECTION_NAME UNSECTION_ID
    Push $R0
    ReadRegStr $R0 HKLM "${REGKEY}\Components" "${SECTION_NAME}"
    StrCmp $R0 1 0 next${UNSECTION_ID}
    !insertmacro SelectSection "${UNSECTION_ID}"
    Goto done${UNSECTION_ID}
next${UNSECTION_ID}:
    !insertmacro UnselectSection "${UNSECTION_ID}"
done${UNSECTION_ID}:
    Pop $R0
!macroend

# Uninstaller sections
Section /o un.Main UNSEC0000
    Delete /REBOOTOK $INSTDIR\lib\*
    RMDir /REBOOTOK $INSTDIR\lib
    RMDir /r /REBOOTOK $INSTDIR\clisp-win
    RMDir /r /REBOOTOK $INSTDIR\xul-win
    RMDir /r /REBOOTOK $INSTDIR\lisp
    RMDir /r /REBOOTOK $INSTDIR\jre
    Delete /REBOOTOK $INSTDIR\CogTool.exe
    
    ; Restore old File Type Association
    ; start of restore script
    !define Index "Line${__LINE__}"
      ReadRegStr $1 HKCR ".cgt" ""
      StrCmp $1 "$(^Name).Project" 0 "${Index}-NoOwn" ; only do this if we own it
        ReadRegStr $1 HKCR ".cgt" "backup_val"
        StrCmp $1 "" 0 "${Index}-Restore" ; if backup="" then delete the whole key
          DeleteRegKey HKCR ".cgt"
        Goto "${Index}-NoOwn"
    "${Index}-Restore:"
          WriteRegStr HKCR ".cgt" "" $1
          DeleteRegValue HKCR ".cgt" "backup_val"
   
    DeleteRegKey HKCR "$(^Name).Project" ;Delete key with association settings
 
    "${Index}-NoOwn:"
    !undef Index
    ; End of Restoring old File Type Association
    
    DeleteRegValue HKLM "${REGKEY}\Components" Main
SectionEnd

Section un.post UNSEC0001
    DeleteRegKey HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\$(^Name)"
    Delete /REBOOTOK "$SMPROGRAMS\$StartMenuGroup\$(^Name) ${Version}.lnk"
    Delete /REBOOTOK "$SMPROGRAMS\$StartMenuGroup\Uninstall $(^Name) ${Version}.lnk"
    Delete /REBOOTOK $INSTDIR\CogTool-uninstall.exe
    DeleteRegValue HKLM "${REGKEY}" StartMenuGroup
    DeleteRegValue HKLM "${REGKEY}" Path
    DeleteRegKey /ifempty HKLM "${REGKEY}\Components"
    DeleteRegKey /ifempty HKLM "${REGKEY}"
    RMDir /REBOOTOK $SMPROGRAMS\$StartMenuGroup
    RMDir /REBOOTOK $INSTDIR
SectionEnd

# Installer functions
Function .onInit
    InitPluginsDir
FunctionEnd

# Uninstaller functions
Function un.onInit
    ReadRegStr $INSTDIR HKLM "${REGKEY}" Path
    ReadRegStr $StartMenuGroup HKLM "${REGKEY}" StartMenuGroup
    !insertmacro SELECT_UNSECTION Main ${UNSEC0000}
FunctionEnd


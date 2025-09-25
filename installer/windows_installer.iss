; Budgeting Tool Windows installer created with Inno Setup
; Build this after running installer/build_installer.py so dist/BudgetingTool exists.

[Setup]
AppId={{C69163E5-2FA5-4F23-9A5E-8D9970A3B8D1}
AppName=Budgeting Tool
AppVersion=1.0.0
AppPublisher=Budgeting Tool Team
DefaultDirName={autopf}\Budgeting Tool
DisableProgramGroupPage=yes
OutputDir=dist
OutputBaseFilename=BudgetingToolSetup
Compression=lzma
SolidCompression=yes
ArchitecturesInstallIn64BitMode=x64
WizardStyle=modern

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked

[Files]
Source: "dist\BudgetingTool\*"; DestDir: "{app}"; Flags: recursesubdirs createallsubdirs replacesameversion

[Icons]
Name: "{group}\Budgeting Tool"; Filename: "{app}\BudgetingTool.exe"
Name: "{commondesktop}\Budgeting Tool"; Filename: "{app}\BudgetingTool.exe"; Tasks: desktopicon

[Run]
Filename: "{app}\BudgetingTool.exe"; Description: "{cm:LaunchProgram,Budgeting Tool}"; Flags: nowait postinstall skipifsilent

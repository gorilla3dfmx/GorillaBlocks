unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Math.Vectors, System.Math,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects3D,
  FMX.Controls3D, FMX.StdCtrls, FMX.Layouts,
  Gorilla.Viewport, Gorilla.Camera, Gorilla.Utils.Timer,
  Gorilla.Material.Default, Gorilla.Cube, Gorilla.SkyBox;

// Basic Tetris constants so CreateGorillaScene compiles (board center usage)
const
  COLS = 10;
  ROWS = 20;

type
  TCellKind = (ckNone, ckI, ckO, ckT, ckS, ckZ, ckJ, ckL);

  TRotation = 0..3;

  TPieceMatrix = array[0..3,0..3] of Boolean;

  TPiece = record
    Kind: TCellKind;
    Matrix: TPieceMatrix;
    X, Y: Integer;
    Rot: TRotation;
  end;

  TBoard = array[0..ROWS-1, 0..COLS-1] of TCellKind;
  TCubes = array[0..ROWS-1, 0..COLS-1] of TGorillaCube;

  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);

  private
    FGorilla : TGorillaViewport;
    FTimer   : TGorillaTimer;

    FTarget  : TDummy;
    FCamera  : TGorillaCamera;

    FBoard : TBoard;
    FCubes : TCubes;

    FActivePiece : TCellKind;
    FActiveX, FActiveY : Integer;
    FLeftHeld,
    FDownHeld,
    FRightHeld: Boolean;
    FLastMove:TDateTime;
    FCurrentPiece:TPiece;

    // Aktives Tetromino als 4 Würfel
    FActiveCubes : array[0..3] of TGorillaCube;
    // Ghost-Würfel für die Vorschau
    FGhostCubes : array[0..3] of TGorillaCube;

    // Gravitation
    FDropTimer : Single;   // Sek. seit letztem Fall
    FDropDelay : Single;   // Sek. pro Zeile (Levelgeschwindigkeit)

    // Zusätzliche Variablen für DAS/ARR
    FMoveTimer : Single; // Timer für horizontale Bewegung
    FMoveDelay : Single; // Verzögerung vor erster Wiederholung (DAS)
    FMoveRate : Single; // Zeit zwischen Wiederholungen (ARR)
    FIsInitialMove : Boolean; // Flag, um erste Bewegung zu markieren
    FGameOver : Boolean; // << Game-Over-Statusvariable

    // --- Neue Variablen für Punkte und Level ---
    FScoreLabel: TLabel;
    FLevelLabel: TLabel;
    FLinesLabel: TLabel;

    FScore: Integer;
    FLevel: Integer;
    FLinesCleared: Integer;

    // --- Neue Variablen für die Animation ---
    FAnimationTimer: Single; // Timer für die Animationsdauer
    FIsAnimating: Boolean; // Statusvariable, die anzeigt, ob eine Animation läuft
    FAnimatedCubes: TArray<TGorillaCube>; // Array für die zu animierenden Würfel
    FAnimatedVelocities: TArray<TVector3D>; // Array für die Startgeschwindigkeiten
    FRemovedLines: TArray<Integer>; // Speichert die Y-Koordinaten der zu löschenden Zeilen

    function TryMove(dx,dy: Integer): Boolean;
    procedure InitBoard;

    procedure SpawnPiece;
    procedure LockPiece;
    procedure StartLineClearAnimation(const Lines: TArray<Integer>);

    procedure RedrawBoard;
    procedure RedrawActive;
    procedure RedrawGhost;
    procedure ClearLines;
    procedure StepGame;
    procedure UpdateStats;

    procedure DoOnTick(Sender: TObject);
  protected
    procedure CreateGorillaScene;
  public
  end;

function Collides(const M: TPieceMatrix; X,Y: Integer; const Board: TBoard): Boolean;
procedure RotatePiece(var P: TPiece; Dir: Integer; const Board: TBoard);

var
  Form1: TForm1;

implementation

{$R *.fmx}

// Array zur Speicherung der Fallgeschwindigkeiten
const DropDelays: array[0..20] of Single =
  (0.8, 0.717, 0.63, 0.55, 0.47, 0.39, 0.31, 0.23, 0.15, 0.1, 0.08, 0.07, 0.06, 0.05, 0.04, 0.03, 0.02, 0.02, 0.02, 0.02, 0.02);
  // (Werte aus original Tetris, für die Level 0-20)

const
  ActivePieceEmission = $55000000;

  PieceColors: array[TCellKind] of TAlphaColor =
    ($33CCCCCC, // None
     $3300FFFF, // Cyan
     $33FFFF00, // Gelb
     $338000FF, // Lila
     $3300FF00, // Grün
     $33FF0000, // Rot
     $330000FF, // Blau
     $33FFA500 // Orange
     );

const
  PieceShapes: array[TCellKind] of TPieceMatrix = (
    // ckNone dummy
    ((False,False,False,False),
     (False,False,False,False),
     (False,False,False,False),
     (False,False,False,False)),

    // I
    ((False,False,False,False),
     (True, True, True, True),
     (False,False,False,False),
     (False,False,False,False)),

    // O
    ((False,True, True,False),
     (False,True, True,False),
     (False,False,False,False),
     (False,False,False,False)),

    // T
    ((False,True, False,False),
     (True, True, True,False),
     (False,False,False,False),
     (False,False,False,False)),

    // S
    ((False, True, True,False),
     (True,  True, False,False),
     (False,False,False,False),
     (False,False,False,False)),

    // Z
    ((True,  True, False,False),
     (False, True, True,False),
     (False,False,False,False),
     (False,False,False,False)),

    // J
    ((True, False,False,False),
     (True, True, True,False),
     (False,False,False,False),
     (False,False,False,False)),

    // L
    ((False,False, True,False),
     (True, True, True,False),
     (False,False,False,False),
     (False,False,False,False))
  );

type
  TKickList = array[0..4] of TPoint;

const
  // JLSTZ-Kicks: FromRot x ToRot
  JLSTZ_Kicks: array[0..3,0..3] of TKickList = (
    ( // From 0
      ( (X:0;Y:0), (X:-1;Y:0), (X:-1;Y:1), (X:0;Y:-2), (X:-1;Y:-2) ), // 0->R
      ( (X:0;Y:0), (X:1;Y:0),  (X:1;Y:1),  (X:0;Y:-2), (X:1;Y:-2) ), // 0->L
      ( (X:0;Y:0), (X:0;Y:0),  (X:0;Y:0),  (X:0;Y:0),  (X:0;Y:0) ),  // 0->2 (unused)
      ( (X:0;Y:0), (X:1;Y:0),  (X:1;Y:1),  (X:0;Y:-2), (X:1;Y:-2) )  // 0->L (duplicate, but defined)
    ),
    ( // From R
      ( (X:0;Y:0), (X:1;Y:0),  (X:1;Y:-1),   (X:0;Y:2),  (X:1;Y:2) ),  // R->0
      ( (X:0;Y:0), (X:1;Y:0),  (X:1;Y:1),  (X:0;Y:-2), (X:1;Y:-2) ), // R->L
      ( (X:0;Y:0), (X:1;Y:0),  (X:1;Y:1),  (X:0;Y:-2), (X:1;Y:-2) ), // R->2
      ( (X:0;Y:0), (X:0;Y:0),  (X:0;Y:0),  (X:0;Y:0),  (X:0;Y:0) )   // R->L (dup)
    ),
    ( // From 2
      ( (X:0;Y:0), (X:1;Y:0),  (X:1;Y:-1), (X:0;Y:2),  (X:1;Y:2) ),  // 2->R
      ( (X:0;Y:0), (X:-1;Y:0), (X:-1;Y:-1),(X:0;Y:2),  (X:-1;Y:2) ), // 2->L
      ( (X:0;Y:0), (X:0;Y:0),  (X:0;Y:0),  (X:0;Y:0),  (X:0;Y:0) ),  // 2->0 (dup)
      ( (X:0;Y:0), (X:-1;Y:0), (X:-1;Y:-1),(X:0;Y:2),  (X:-1;Y:2) )  // 2->L
    ),
    ( // From L
      ( (X:0;Y:0), (X:-1;Y:0), (X:-1;Y:1), (X:0;Y:-2), (X:-1;Y:-2) ), // L->0
      ( (X:0;Y:0), (X:0;Y:0),  (X:0;Y:0),  (X:0;Y:0),  (X:0;Y:0) ),  // L->R (dup)
      ( (X:0;Y:0), (X:-1;Y:0), (X:-1;Y:1), (X:0;Y:-2), (X:-1;Y:-2) ), // L->2
      ( (X:0;Y:0), (X:0;Y:0),  (X:0;Y:0),  (X:0;Y:0),  (X:0;Y:0) )   // L->L
    )
  );

const
  I_Kicks: array[0..3,0..3] of TKickList = (
    ( // From 0
      ( (X:0;Y:0), (X:-2;Y:0), (X:1;Y:0), (X:-2;Y:-1), (X:1;Y:2) ), // 0->R
      ( (X:0;Y:0), (X:2;Y:0),  (X:-1;Y:0),(X:2;Y:1),   (X:-1;Y:-2)), // 0->L
      ( (X:0;Y:0), (X:0;Y:0),  (X:0;Y:0), (X:0;Y:0),   (X:0;Y:0) ), // 0->2
      ( (X:0;Y:0), (X:-1;Y:0), (X:2;Y:0), (X:-1;Y:2),  (X:2;Y:-1) ) // 0->L
    ),
    ( // From R
      ( (X:0;Y:0), (X:2;Y:0),  (X:-1;Y:0),(X:2;Y:1),   (X:-1;Y:-2)), // R->0
      ( (X:0;Y:0), (X:-1;Y:0), (X:2;Y:0), (X:-1;Y:2),  (X:2;Y:-1) ), // R->2
      ( (X:0;Y:0), (X:0;Y:0),  (X:0;Y:0), (X:0;Y:0),   (X:0;Y:0) ), // R->L
      ( (X:0;Y:0), (X:1;Y:0),  (X:-2;Y:0),(X:1;Y:-2),  (X:-2;Y:1) )  // R->L
    ),
    ( // From 2
      ( (X:0;Y:0), (X:1;Y:0),  (X:-2;Y:0),(X:1;Y:-2),  (X:-2;Y:1) ), // 2->R
      ( (X:0;Y:0), (X:-2;Y:0), (X:1;Y:0), (X:-2;Y:-1), (X:1;Y:2) ),  // 2->L
      ( (X:0;Y:0), (X:0;Y:0),  (X:0;Y:0), (X:0;Y:0),   (X:0;Y:0) ),  // 2->0
      ( (X:0;Y:0), (X:2;Y:0),  (X:-1;Y:0),(X:2;Y:1),   (X:-1;Y:-2))  // 2->L
    ),
    ( // From L
      ( (X:0;Y:0), (X:-1;Y:0), (X:2;Y:0), (X:-1;Y:2),  (X:2;Y:-1) ), // L->0
      ( (X:0;Y:0), (X:1;Y:0),  (X:-2;Y:0),(X:1;Y:-2),  (X:-2;Y:1) ), // L->R
      ( (X:0;Y:0), (X:0;Y:0),  (X:0;Y:0), (X:0;Y:0),   (X:0;Y:0) ), // L->2
      ( (X:0;Y:0), (X:0;Y:0),  (X:0;Y:0), (X:0;Y:0),   (X:0;Y:0) )   // L->L
    )
  );

procedure RotatePiece(var P: TPiece; Dir: Integer; const Board: TBoard);
var
  OldRot, NewRot: TRotation;
  NewMat: TPieceMatrix;
  x,y: Integer;
  Kicks: TKickList;
  dx,dy: Integer;
begin
  OldRot := P.Rot;
  NewRot := (OldRot + Dir + 4) mod 4;

  // Matrix drehen
  for y := 0 to 3 do
    for x := 0 to 3 do
      if Dir=1 then // rechts
        NewMat[x,3-y] := P.Matrix[y,x]
      else           // links
        NewMat[3-x,y] := P.Matrix[y,x];

  // Kick-Tabelle auswählen
  if P.Kind = ckI then
    Kicks := I_Kicks[P.Rot, NewRot]
  else
    Kicks := JLSTZ_Kicks[P.Rot, NewRot];

  // Kick-Offsets testen
  for var k in Kicks do
  begin
    if not Collides(NewMat, P.X+k.X, P.Y+k.Y, Board) then
    begin
      P.Matrix := NewMat;
      P.Rot := NewRot;
      P.X := P.X + k.X;
      P.Y := P.Y + k.Y;
      Exit;
    end;
  end;
end;

function Collides(const M: TPieceMatrix; X,Y: Integer; const Board: TBoard): Boolean;
var i,j: Integer;
begin
  for j := 0 to 3 do
    for i := 0 to 3 do
      if M[j,i] then
      begin
        if (X+i<0) or (X+i>=COLS) or (Y+j>=ROWS) then
          Exit(True);
        if (Y+j>=0) and (Board[Y+j,X+i]<>ckNone) then
          Exit(True);
      end;
  Result := False;
end;

function TForm1.TryMove(dx,dy: Integer): Boolean;
begin
  // Prüfe Kollision am Ziel
  Result := not Collides(FCurrentPiece.Matrix, FCurrentPiece.X + dx, FCurrentPiece.Y + dy, FBoard);
  if Result then
  begin
    Inc(FCurrentPiece.X, dx);
    Inc(FCurrentPiece.Y, dy);
    RedrawActive;
  end
  else
  begin
    // Wenn die Bewegung fehlschlägt UND es sich um eine Bewegung nach unten handelt,
    // UND die Y-Koordinate noch in der "Startzone" ist (über dem Spielfeld)
    if (dy > 0) and (FCurrentPiece.Y < 0) then
    begin
      FGameOver := True;
      // Aktive Würfel verstecken
      for var i := 0 to 3 do
        FActiveCubes[i].Visible := False;
      ShowMessage('Game Over!');
    end;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Randomize;

  // Spielzustand initialisieren
  FScore := 0;
  FLevel := 0;
  FLinesCleared := 0;

  FDropDelay := 0.9; // langsamer Start
  FDropTimer := 0;

  // Initialisiere die DAS/ARR-Werte
  FMoveDelay := 0.2; // 200 ms Verzögerung
  FMoveRate := 0.05; // 50 ms zwischen Wiederholungen
  FMoveTimer := 0;
  FIsInitialMove := False;
  FGameOver := False; // << Spielstart: Nicht Game Over

  // create viewport at runtime and fill the form
  FGorilla := TGorillaViewport.Create(Self);
  FGorilla.Parent := Self;
  FGorilla.Align := TAlignLayout.Contents;
  FGorilla.UsingDesignCamera := False;
  FGorilla.EmissiveBlur := 3;

  var LSkyBox := TGorillaSkyBox.Create(FGorilla);
  LSkyBox.Parent := FGorilla;

  // manual rendering via GorillaTimer (fixed ~60 fps)
  FTimer := TGorillaTimer.Create;
  FTimer.Interval := 16; // ~60 FPS
  FTimer.OnTimer := DoOnTick;
  FTimer.Start;

  // UI-Labels erstellen
  FScoreLabel := TLabel.Create(FGorilla);
  FScoreLabel.Parent := FGorilla;
  FScoreLabel.Align := TAlignLayout.Top;
  FScoreLabel.Margins.Left := 10;
  FScoreLabel.TextSettings.Font.Size := 14;
  FScoreLabel.TextSettings.Font.Style := [TFontStyle.fsBold];
  FScoreLabel.TextSettings.Font.Family := 'Arial';
  FScoreLabel.TextSettings.FontColor := TAlphaColors.White;

  FLevelLabel := TLabel.Create(FGorilla);
  FLevelLabel.Parent := FGorilla;
  FLevelLabel.Align := TAlignLayout.Top;
  FLevelLabel.Margins.Left := 10;
  FLevelLabel.Margins.Top := 30;
  FLevelLabel.TextSettings.Font.Size := 14;
  FLevelLabel.TextSettings.FontColor := TAlphaColors.White;

  FLinesLabel := TLabel.Create(FGorilla);
  FLinesLabel.Parent := FGorilla;
  FLinesLabel.Align := TAlignLayout.Top;
  FLinesLabel.Margins.Left := 10;
  FLinesLabel.Margins.Top := 50;
  FLinesLabel.TextSettings.Font.Size := 14;
  FLinesLabel.TextSettings.FontColor := TAlphaColors.White;

  CreateGorillaScene;
  InitBoard;
  SpawnPiece;

  UpdateStats; // Neue Prozedur zum Aktualisieren der Labels
  FDropDelay := DropDelays[FLevel];
end;

procedure TForm1.CreateGorillaScene;
begin
  // Create a dummy as camera target at the board center
  FTarget := TDummy.Create(FGorilla);
  FTarget.Parent := FGorilla;
  FTarget.Position.Point := Point3D( COLS/2, ROWS/2, 0 ); // forward declare via consts below

  // Create a Gorilla camera as child of target (orbit-style) and link to viewport
  FCamera := TGorillaCamera.Create(FTarget);
  FCamera.Parent := FTarget;
  FCamera.Position.Point := Point3D(0, 0, 28); // offset back on Z
  FCamera.FOV := 45;
  FCamera.Target := FTarget;    // << link target so it always looks at board center

  FGorilla.Camera := FCamera;   // << link camera to viewport
  FGorilla.UsingDesignCamera := false;

  for var i := 0 to 3 do
  begin
    FActiveCubes[i] := TGorillaCube.Create(FGorilla);
    FActiveCubes[i].Parent := FGorilla;
    FActiveCubes[i].SetSize(0.98,0.98,0.98);
    FActiveCubes[i].Visible := False;
    // Material wird in RedrawActive gesetzt
  end;

// Erstelle die Ghost-Würfel
  for var i := 0 to 3 do
  begin
    FGhostCubes[i] := TGorillaCube.Create(FGorilla);
    FGhostCubes[i].Parent := FGorilla;
    FGhostCubes[i].SetSize(0.98,0.98,0.98);
    FGhostCubes[i].Visible := False;
    FGhostCubes[i].SetOpacityValue(0.5);
    // Setze ein Material mit geringer Leuchtkraft für den Ghost
    var mat := TGorillaDefaultMaterialSource.Create(FGorilla);
    mat.Parent := FGorilla;
    mat.Diffuse := $FF888888;
    mat.Emissive := 0; // Ein heller, transparenter Schimmer
    FGhostCubes[i].MaterialSource := mat;
  end;
end;

procedure TForm1.UpdateStats;
begin
  FScoreLabel.Text := 'Score: ' + IntToStr(FScore);
  FLevelLabel.Text := 'Level: ' + IntToStr(FLevel);
  FLinesLabel.Text := 'Lines: ' + IntToStr(FLinesCleared);
end;

procedure TForm1.DoOnTick(Sender: TObject);
const DT = 1/60;
var
  i: Integer;
  pos: TVector3D;
begin
(*
  // Wenn eine Animation läuft, nur diese verarbeiten
  if FIsAnimating then
  begin
    FAnimationTimer := FAnimationTimer + DT;

    // Animation der explodierenden Würfel
    for i := 0 to High(FAnimatedCubes) do
    begin
      if Assigned(FAnimatedCubes[i]) then
      begin
        // Aktuelle Position
        pos := FAnimatedCubes[i].Position.Point;
        // Position mit Geschwindigkeit aktualisieren
        pos.X := pos.X + FAnimatedVelocities[i].X * DT * 5;
        pos.Y := pos.Y + FAnimatedVelocities[i].Y * DT * 5;
        pos.Z := pos.Z + FAnimatedVelocities[i].Z * DT * 5;
        FAnimatedCubes[i].Position.Point := pos;

        // Skalierung verringern, um das Verschwinden zu simulieren
        FAnimatedCubes[i].SetSize(FAnimatedCubes[i].Scale.X - DT * 1.5, FAnimatedCubes[i].Scale.Y - DT * 1.5, FAnimatedCubes[i].Scale.Z - DT * 1.5);

        // Würfel langsam transparenter machen
        FAnimatedCubes[i].SetOpacityValue(FAnimatedCubes[i].Opacity - DT * 2);
      end;
    end;

    // Animation beenden und Spielfeld aktualisieren, wenn der Timer abläuft
    if FAnimationTimer >= 0.5 then // 0.5 Sekunden Explosionsdauer
    begin
      FIsAnimating := False;
      RedrawBoard;
      UpdateStats;
      SpawnPiece;
    end;
    Exit; // Timer-Prozedur verlassen, um Gravitation zu überspringen
  end;
*)
  // Animation beenden und Spielfeld aktualisieren, wenn der Timer abläuft
  if FAnimationTimer >= 0.5 then
  begin
    FIsAnimating := False;

    // Freigabe der animierten Würfel
    for i := 0 to High(FAnimatedCubes) do
    begin
      if Assigned(FAnimatedCubes[i]) then
      begin
        FAnimatedCubes[i].Free;
        FAnimatedCubes[i] := nil;
      end;
    end;
    SetLength(FAnimatedCubes, 0);
    SetLength(FAnimatedVelocities, 0);

    RedrawBoard;
    UpdateStats;
    SpawnPiece;
  end;

  // --- Gravitations-Logik und DAS/ARR wie zuvor ---
  if FDownHeld then
  begin
    if not TryMove(0,1) then
      LockPiece;
  end
  else
  begin
    FDropTimer := FDropTimer + DT;
    if FDropTimer >= FDropDelay then
    begin
      if not TryMove(0,1) then
        LockPiece;
      FDropTimer := 0;
    end;
  end;

  if FLeftHeld or FRightHeld then
  begin
    FMoveTimer := FMoveTimer + DT;
    if not FIsInitialMove then
    begin
      FIsInitialMove := True;
      if FLeftHeld then TryMove(1, 0);
      if FRightHeld then TryMove(-1, 0);
      FMoveTimer := 0;
    end
    else
    begin
      var delay := FMoveDelay;
      if FMoveTimer >= delay then
      begin
        if FLeftHeld then TryMove(1, 0);
        if FRightHeld then TryMove(-1, 0);
        FMoveTimer := delay - FMoveRate;
      end;
    end;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if Assigned(FTimer) then
  begin
    if FTimer.Started then
    begin
      FTimer.Terminate;
      FTimer.WaitFor;
    end;
    FTimer.Free;
  end;
end;

procedure TForm1.InitBoard;
var x,y: Integer; cube: TGorillaCube; mat: TGorillaDefaultMaterialSource;
begin
  for y := 0 to ROWS-1 do
    for x := 0 to COLS-1 do
    begin
      FBoard[y,x] := ckNone;

      cube := TGorillaCube.Create(FGorilla);
      cube.Parent := FGorilla;
      cube.Position.Point := Point3D(x, y, 0);
      cube.SetSize(0.94,0.94,0.94);

      mat := TGorillaDefaultMaterialSource.Create(FGorilla);
      mat.Parent := FGorilla;
      mat.Diffuse  := $FF0D1224;   // dunkel
      mat.Emissive := $00000000;   // aus
      cube.MaterialSource := mat;

      cube.Visible := False;
      FCubes[y,x] := cube;
    end;
end;

procedure TForm1.RedrawBoard;
var x,y: Integer; mat: TGorillaDefaultMaterialSource;
begin
  for y := 0 to ROWS-1 do
    for x := 0 to COLS-1 do
    begin
      if FBoard[y,x]<>ckNone then
      begin
        if not Assigned(FCubes[y,x]) then
        begin
          // Falls ein Würfel fehlt (z.B. nach dem Verschieben), erstelle ihn neu
          FCubes[y,x] := TGorillaCube.Create(FGorilla);
          FCubes[y,x].Parent := FGorilla;
          FCubes[y,x].Position.Point := Point3D(x, y, 0);
          FCubes[y,x].SetSize(0.94,0.94,0.94);
        end;

        FCubes[y,x].Visible := True;
        mat := TGorillaDefaultMaterialSource(FCubes[y,x].MaterialSource);
        if not Assigned(mat) then
        begin
          mat := TGorillaDefaultMaterialSource.Create(FGorilla);
          mat.Parent := FGorilla;
          mat.Diffuse  := $FF0D1224;
          FCubes[y,x].MaterialSource := mat;
        end;
        mat.Emissive := PieceColors[FBoard[y,x]];
      end
      else
      begin
        if Assigned(FCubes[y,x]) then
        begin
          FCubes[y,x].Visible := False;
        end;
      end;
    end;
end;

procedure TForm1.SpawnPiece;
begin
  // TODO: später Bag7 – erstmal zufällig:
  FCurrentPiece.Kind := TCellKind(Ord(ckI) + Random(7)); // ckI..ckL
  FCurrentPiece.Matrix := PieceShapes[FCurrentPiece.Kind];
  FCurrentPiece.Rot := 0;
  FCurrentPiece.X := (COLS div 2) - 2; // zentriert für 4x4-Matrix
  FCurrentPiece.Y := -2;               // leicht über Feld

  // **Game-Over-Erkennung**: Prüfe, ob das neue Stück kollidiert
  if Collides(FCurrentPiece.Matrix, FCurrentPiece.X, FCurrentPiece.Y, FBoard) then
  begin
    FGameOver := True; // Setze den Game-Over-Status
    ShowMessage('Game Over! Your Score: ' + IntToStr(FScore));

    // Spielzustand zurücksetzen
    FScore := 0;
    FLevel := 0;
    FLinesCleared := 0;
    FDropDelay := DropDelays[0];

    // Reset bei Game Over (einfach) – später schöner
    FillChar(FBoard, SizeOf(FBoard), 0);
    RedrawBoard;
    UpdateStats;
  end
  else
  begin
    RedrawActive; // Zeichne nur, wenn das Spiel weitergeht
  end;
end;

procedure TForm1.StepGame;
begin
  // einfache Gravitation: ein Feld runter
  if (FActiveY<ROWS-1) and (FBoard[FActiveY+1, FActiveX]=ckNone) then
    Inc(FActiveY)
  else
  begin
    // Stück fixieren
    FBoard[FActiveY,FActiveX] := FActivePiece;
    SpawnPiece;
  end;

  RedrawBoard;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if FGameOver and (Key = vkReturn) then // Beispiel: Neustart mit Enter
  begin
    FGameOver := False;
    FillChar(FBoard, SizeOf(FBoard), 0); // Spielfeld leeren
    RedrawBoard;
    SpawnPiece;
    Exit; // Beende die Prozedur, um andere Tastenbefehle zu ignorieren
  end;

  case Key of
    vkLeft:
      begin
        FLeftHeld := True;
        FIsInitialMove := False;
        FMoveTimer := 0;
      end;
    vkRight:
      begin
        FRightHeld := True;
        FIsInitialMove := False;
        FMoveTimer := 0;
      end;
    vkDown: FDownHeld := True;

    vkUp: begin RotatePiece(FCurrentPiece, +1, FBoard); RedrawActive; end;
    vkZ: begin RotatePiece(FCurrentPiece, -1, FBoard); RedrawActive; end;

    vkSpace: begin // Hard Drop
      while TryMove(0,1) do ;
      LockPiece;
    end;
  end;
end;

procedure TForm1.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  case Key of
    vkLeft:
      begin
        FLeftHeld := False;
        FIsInitialMove := False;
        FMoveTimer := 0;
      end;
    vkRight:
      begin
        FRightHeld := False;
        FIsInitialMove := False;
        FMoveTimer := 0;
      end;
    vkDown: FDownHeld := False;
  end;
end;

function CellsOf(const M: TPieceMatrix): TArray<TPoint>;
var i,j,c: Integer;
begin
  SetLength(Result,4); c := 0;
  for j:=0 to 3 do
    for i:=0 to 3 do
      if M[j,i] then begin Result[c]:=Point(i,j); Inc(c); end;
end;

procedure TForm1.RedrawActive;
var cells: TArray<TPoint>;
    i: Integer; p: TPoint; col: TAlphaColor;
    mat: TGorillaDefaultMaterialSource;
begin
  cells := CellsOf(FCurrentPiece.Matrix);
  col := PieceColors[FCurrentPiece.Kind];

  for i:=0 to 3 do
  begin
    p := cells[i];
    FActiveCubes[i].Visible := True;
    // Board-Koordinate -> 3D: oben = ROWS-1
    FActiveCubes[i].Position.Point :=
      Point3D(FCurrentPiece.X + p.X, (FCurrentPiece.Y + p.Y), 0);

    if not Assigned(FActiveCubes[i].MaterialSource) then
    begin
      mat := TGorillaDefaultMaterialSource.Create(FGorilla);
      mat.Parent := FGorilla;
      mat.Diffuse  := $FF0D1224;
      // Emissive-Alpha = Intensität (~60%)
      mat.Emissive := (col and $00FFFFFF) or ActivePieceEmission;
      FActiveCubes[i].MaterialSource := mat;
    end
    else
    begin
      mat := TGorillaDefaultMaterialSource(FActiveCubes[i].MaterialSource);
      mat.Emissive := (col and $00FFFFFF) or ActivePieceEmission;
    end;
  end;

  RedrawGhost;
end;

procedure TForm1.LockPiece;
var cells: TArray<TPoint>;
    p: TPoint;
    i: Integer;
begin
  cells := CellsOf(FCurrentPiece.Matrix);
  for i:=0 to 3 do
  begin
    p := cells[i];
    if FCurrentPiece.Y + p.Y >= 0 then
      FBoard[FCurrentPiece.Y + p.Y, FCurrentPiece.X + p.X] := FCurrentPiece.Kind;
  end;

  // Aktive Würfel verstecken bis zum nächsten Teil
  for i:=0 to 3 do FActiveCubes[i].Visible := False;
  // Ghost-Würfel verstecken
  for i := 0 to 3 do FGhostCubes[i].Visible := False;

  RedrawBoard;
  ClearLines; // definiere unten
end;

procedure TForm1.ClearLines;
var
  y, x: Integer;
  full: Boolean;
  linesToClear: TArray<Integer>;
  yOffset: Integer;
  found: Boolean;
begin
  SetLength(linesToClear, 0);

  // Schritt 1: Finde alle vollen Reihen und speichere sie
  for y := ROWS - 1 downto 0 do
  begin
    full := True;
    for x := 0 to COLS - 1 do
    begin
      if FBoard[y, x] = ckNone then
      begin
        full := False;
        Break;
      end;
    end;

    if full then
    begin
      linesToClear := linesToClear + [y];
    end;
  end;

  if Length(linesToClear) > 0 then
  begin
    // Schritt 2: Punkte vergeben und Level erhöhen
    case Length(linesToClear) of
      1: FScore := FScore + 40 * (FLevel + 1);
      2: FScore := FScore + 100 * (FLevel + 1);
      3: FScore := FScore + 300 * (FLevel + 1);
      4: FScore := FScore + 1200 * (FLevel + 1);
    end;

    FLinesCleared := FLinesCleared + Length(linesToClear);
    if FLinesCleared div 10 > FLevel then
    begin
      Inc(FLevel);
      if FLevel <= High(DropDelays) then
        FDropDelay := DropDelays[FLevel];
    end;

    // Schritt 3: Die Blöcke und die 3D-Würfel verschieben
    yOffset := 0;
    // Durchlaufe das Spielfeld von unten nach oben
    for y := ROWS - 1 downto 0 do
    begin
      // Überprüfe, ob die aktuelle Zeile gelöscht wird
      found := False;
      for var clearedLine in linesToClear do
      begin
        if y = clearedLine then
        begin
          found := True;
          Break;
        end;
      end;

      if found then
      begin
        // Wenn die Zeile gelöscht wird, erhöhe den Offset
        Inc(yOffset);
        // Setze die Blöcke in dieser Zeile im FBoard und die 3D-Würfel zurück
        for x := 0 to COLS - 1 do
        begin
          FBoard[y, x] := ckNone;
          // Entferne den Würfel vollständig
          if Assigned(FCubes[y,x]) then
          begin
            FCubes[y,x].Free;
            FCubes[y,x] := nil;
          end;
        end;
      end
      else if yOffset > 0 then
      begin
        // Wenn die Zeile nicht gelöscht wird, aber ein Offset vorhanden ist,
        // verschiebe die Zeile und den 3D-Würfel um den Offset nach unten
        for x := 0 to COLS - 1 do
        begin
          // Verschiebe die logischen Daten im FBoard-Array
          FBoard[y + yOffset, x] := FBoard[y, x];
          FBoard[y, x] := ckNone;

          // Verschiebe den 3D-Würfel im FCubes-Array und aktualisiere seine Position
          if Assigned(FCubes[y, x]) then
          begin
            FCubes[y + yOffset, x] := FCubes[y, x];
            FCubes[y + yOffset, x].Position.Y := y + yOffset;
            FCubes[y, x] := nil; // Setze den alten Platz auf nil
          end;
        end;
      end;
    end;

    // Schritt 4: Starte die Animation
    StartLineClearAnimation(linesToClear);
  end
  else
  begin
    // Wenn keine Zeilen gelöscht wurden, sofort das nächste Teil spawnen
    if not FGameOver then
      SpawnPiece;
  end;
end;

procedure TForm1.RedrawGhost;
var cells: TArray<TPoint>;
    i: Integer;
    p: TPoint;
    GhostPiece: TPiece;
begin
  // Kopiere den aktuellen Block
  GhostPiece := FCurrentPiece;

  // Finde die niedrigste Position, an der der Block landen kann
  while not Collides(GhostPiece.Matrix, GhostPiece.X, GhostPiece.Y + 1, FBoard) do
    Inc(GhostPiece.Y);

  cells := CellsOf(GhostPiece.Matrix);

  // Aktualisiere die Position und Sichtbarkeit der Ghost-Cubes
  for i := 0 to 3 do
  begin
    p := cells[i];
    FGhostCubes[i].Visible := True;
    FGhostCubes[i].Position.Point := Point3D(GhostPiece.X + p.X, (GhostPiece.Y + p.Y), 0);
  end;
end;

procedure TForm1.StartLineClearAnimation(const Lines: TArray<Integer>);
var
  y, x: Integer;
  cube: TGorillaCube;
  idx: Integer;
begin
  FIsAnimating := True;
  FAnimationTimer := 0;

  // Blöcke aus den zu löschenden Reihen in das Animations-Array verschieben
  SetLength(FAnimatedCubes, Length(Lines) * COLS);
  SetLength(FAnimatedVelocities, Length(Lines) * COLS);

  idx := 0;
  for y := Low(Lines) to High(Lines) do
  begin
    for x := 0 to COLS - 1 do
    begin
      // Holen Sie sich den Würfel, bevor er entfernt wird
      cube := FCubes[y, x];
      if Assigned(cube) and cube.Visible then
      begin
        FAnimatedCubes[idx] := cube;
        // Zufällige Startgeschwindigkeit für eine Explosion
        FAnimatedVelocities[idx] := Vector3D(RandomRange(-5, 5) / 10, RandomRange(-5, 5) / 10, RandomRange(5, 10) / 10);
        Inc(idx);
      end;
    end;
  end;
  SetLength(FAnimatedCubes, idx);
  SetLength(FAnimatedVelocities, idx);

  // Die Würfel in den gelöschten Zeilen nun aus dem FCubes-Array entfernen
  for y := Low(Lines) to High(Lines) do
    for x := 0 to COLS - 1 do
      FCubes[y,x] := nil;
end;

end.
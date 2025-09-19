unit MainForm;

interface

{$DEFINE AUDIO}
{$DEFINE SKYBOX}

{$IFDEF ANDROID}
  {$UNDEF AUDIO}
  {$UNDEF SKYBOX}
{$ENDIF}

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Math.Vectors, System.Math, System.IOUtils, System.SyncObjs, FMX.Types3D,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects3D,
  FMX.Controls3D, FMX.StdCtrls, FMX.Layouts, FMX.Objects, FMX.Controls.Presentation,
  FMX.Gestures,
{$IFDEF AUDIO}
  Gorilla.Audio.FMOD,
{$ENDIF}
  Gorilla.Viewport, Gorilla.Camera, Gorilla.Utils.Timer, Gorilla.Light,
  Gorilla.Material.Default, Gorilla.Cube, Gorilla.SkyBox, Gorilla.Plane,
  Gorilla.DefTypes, Gorilla.Material.Lambert, Gorilla.Material.Blinn;

// Basic Tetris constants so CreateGorillaScene compiles (board center usage)
const
  ROWS       = 20;   // Anzahl Reihen im Spielfeld
  COLS       = 10;   // Anzahl Spalten im Spielfeld
  CUBE_SIZE  = 1.0;  // Kantenlänge eines Blocks in deiner 3D-Szene
  CUBE_SPACE = 0.05; // kleiner Abstand zwischen den Blöcken

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
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Rectangle1: TRectangle;
    ScoreLabel: TLabel;
    LevelLabel: TLabel;
    LinesLabel: TLabel;
    Image4: TImage;
    FinalScoreLabel: TLabel;
    GestureManager1: TGestureManager;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);

    procedure ViewportTap(Sender: TObject; const Point: TPointF);
    procedure ViewportGesture(Sender: TObject; const EventInfo: TGestureEventInfo;
      var Handled: Boolean);

  private
    FGorilla : TGorillaViewport;
    FTimer   : TGorillaTimer;
    FAssetsPath : String;

    FTarget  : TDummy;
    FCamera  : TGorillaCamera;
    FLight   : TGorillaLight;

    FBoard : TBoard;
    FCubes : TCubes;

    FActivePiece : TCellKind;
    FActiveX, FActiveY : Integer;
    FLeftHeld,
    FDownHeld,
    FRightHeld : Boolean;

    FHardDropExtraPoints,
    FHardDrop  : Boolean;

    FLastMove  : TDateTime;
    FWithGesture : Boolean;
    FCurrentPiece :TPiece;

    // Aktives Tetromino als 4 Würfel
    FActiveCubes : array[0..3] of TGorillaCube;
    // Ghost-Würfel für die Vorschau
    FGhostCubes : array[0..3] of TGorillaCube;

    // Gravitation
    FPieceDropTimer : Single;   // Sek. seit letztem Fall
    FPieceDropDelay : Single;   // Sek. pro Zeile (Levelgeschwindigkeit)

    // Zusätzliche Variablen für DAS/ARR
    FMoveTimer : Single; // Timer für horizontale Bewegung
    FMoveDelay : Single; // Verzögerung vor erster Wiederholung (DAS)
    FMoveRate : Single; // Zeit zwischen Wiederholungen (ARR)
    FMoveIntensity : Integer;
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
    FAnimatedBasePos: TArray<TPoint3D>; // Ursprungspositionen für Vibration
    FAnimatedVibrateDirs: TArray<TVector3D>; // Richtungen der Vibration pro Würfel
    FAnimatedPhases: TArray<Single>; // Phasen für die Sinus-Vibration
    FExplosionTriggered: Boolean; // interne Flag, ob die Explosion ausgelöst wurde
    FRemovedLines: TArray<Integer>; // Speichert die Y-Koordinaten der zu löschenden Zeilen

    // Next-piece preview
    FNextPieces: array[0..1] of TCellKind;
    FPreviewCubes: array[0..1, 0..3] of TGorillaCube;

    // Bag7 Randomizer
    FBag: array[0..6] of TCellKind;
    FBagIndex: Integer; // Index auf das nächste Element in der Bag (0..6)

  {$IFDEF AUDIO}
    // Music and effects
    FAudioPlayer : TGorillaFMODAudioManager;
  {$ENDIF}

    // --- Shake-Animation für HardDrop ---
    FIsShaking: Boolean;
    FShakeTimer: Single;
    FShakeDuration: Single;
    FShakeOscillations: Integer; // wie oft schwingt es hin und her
    FShakingCubes: TArray<TGorillaCube>;
    FShakeOrigPos: TArray<TVector3D>;
    FShakeAmplitudes: TArray<TVector3D>;
    FShakeBaseFreqs: TArray<TVector3D>;

    // --- Camera Shake ---
    FCameraOrigPos: TVector3D;
    FCameraShakeEnabled: Boolean;
    FCameraShakeAmplitude: TVector3D;
    FCameraShakeBaseFreq: TVector3D;

    // --- Drop-Animation nach Line Clear ---
    FIsDropping: Boolean;
    FDropTimer: Single;
    FDropDuration: Single;
    FDroppingCubes: TArray<TGorillaCube>;
    FDropStartPos: TArray<TVector3D>;
    FDropEndPos: TArray<TVector3D>;
    FDropDelay: TArray<Single>; // individuelle Verzögerung pro Würfel

    procedure ShuffleBag;
    procedure RefillBag;
    function DrawFromBag: TCellKind;

    function TryMove(dx,dy: Integer): Boolean;
    procedure InitBoard;

    procedure SpawnPiece;
    procedure LockPiece;

    procedure RedrawBoard;
    procedure RedrawActive;
    procedure RedrawGhost;
    procedure ClearLines;
    procedure StepGame;
    procedure UpdateStats;
    procedure UpdatePreview;

    procedure StartLineClearAnimation(const Lines: TArray<Integer>);
    procedure StartShakeBoard;
    procedure StartDropAnimation;

    procedure DoOnTick(Sender: TObject);

    procedure HandleGameOver; // zentrale, thread-sichere GameOver-Routine
    procedure Clear();

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
const PieceDropDelays: array[0..20] of Single =
  (0.8, 0.717, 0.63, 0.55, 0.47, 0.39, 0.31, 0.23, 0.15, 0.1, 0.08, 0.07, 0.06, 0.05, 0.04, 0.03, 0.02, 0.02, 0.02, 0.02, 0.02);
  // (Werte aus original Tetris, für die Level 0-20)

const
  ActivePieceEmission = $55000000;

  PieceColors: array[TCellKind] of TAlphaColor =
    ($44CCCCCC, // None
     $448ecae6, // Cyan
     $44ffee32, // Gelb
     $44c86bfa, // Lila
     $4490be6d, // Grün
     $44f94144, // Rot
     $44abc4ff, // Blau
     $44f8961e // Orange
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

function TForm1.TryMove(dx, dy: Integer): Boolean;
begin
  dx := dx * FMoveIntensity;

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
      // Aktive Würfel verstecken
      for var i := 0 to 3 do
        if Assigned(FActiveCubes[i]) then
          FActiveCubes[i].Visible := False;

      // Thread-sichere GameOver-Handling
      HandleGameOver;
      // wichtig: kein weiteres Fortfahren aus dieser Bewegung
      Exit;
    end;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Randomize;

{$IFDEF MSWINDOWS}
  FAssetsPath := '';
{$ENDIF}
{$IFDEF LINUX}
  FAssetsPath := '';
{$ENDIF}
{$IFDEF ANDROID}
  FAssetsPath := IncludeTrailingPathDelimiter(System.IOUtils.TPath.GetHomePath());
{$ENDIF}

{$IFDEF AUDIO}
  FAudioPlayer := TGorillaFMODAudioManager.Create(Self);
  FAudioPlayer.AutoUpdate := true;
  var LSoundItem := FAudioPlayer.LoadSoundItemFromFile(FAssetsPath + 'tetris_style.wav');
  LSoundItem.Loop := true;
  LSoundItem.LoopCount := -1;
  LSoundItem.Play();
{$ENDIF}

  // Spielzustand initialisieren
  FScore := 0;
  FLevel := 0;
  FLinesCleared := 0;

  FPieceDropDelay := 0.9; // langsamer Start
  FPieceDropTimer := 0;

  // Initialisiere die DAS/ARR-Werte
  FMoveDelay := 0.2; // 200 ms Verzögerung
  FMoveRate := 0.05; // 50 ms zwischen Wiederholungen
  FMoveTimer := 0;
  FMoveIntensity := 1;
  FIsInitialMove := False;
  FGameOver := False; // << Spielstart: Nicht Game Over

  FIsShaking := False;
  FShakeTimer := 0;
  FShakeDuration := 0.5;
  FShakeOscillations := 4;

  // Camera-Shake default einschalten (kann später ausgeschaltet werden)
  FCameraShakeEnabled := True;
  // Standard-Amplitude: etwas kleiner als Würfel-Amplitude, kann angepasst werden
  FCameraShakeAmplitude := Vector3D(0.25, 0.25, 0.0); // X,Y,Z in scene units
  FCameraShakeBaseFreq := Vector3D(1.0, 1.2, 0.9);    // Basisfrequenzen (multipliziert mit Oscillations)

  // create viewport at runtime and fill the form
  FGorilla := TGorillaViewport.Create(Self);
  FGorilla.Parent := Self;
  FGorilla.Align := TAlignLayout.Contents;
  FGorilla.UsingDesignCamera := False;
  FGorilla.EmissiveBlur := 3;

  FGorilla.Touch.GestureManager := GestureManager1;
  FGorilla.Touch.StandardGestures := [TStandardGesture.sgLeft,
    TStandardGesture.sgRight, TStandardGesture.sgUp, TStandardGesture.sgDown];
  FGorilla.Touch.InteractiveGestures := [TInteractiveGesture.LongTap];
  FGorilla.OnGesture := ViewportGesture;
  FGorilla.OnTap := ViewportTap;

  // manual rendering via GorillaTimer (fixed ~60 fps)
  FTimer := TGorillaTimer.Create;
  FTimer.Interval := 16; // ~60 FPS
  FTimer.OnTimer := DoOnTick;
  FTimer.Start;

  Rectangle1.Parent := FGorilla;
  FScoreLabel := ScoreLabel;
  FLevelLabel := LevelLabel;
  FLinesLabel := LinesLabel;

  Image4.Parent := FGorilla;
  Image4.Visible := false;

  CreateGorillaScene;
  InitBoard;

  // Bag7 initialisieren
  RefillBag;

  // Next-Pieces initialisieren aus Bag
  FNextPieces[0] := DrawFromBag;
  FNextPieces[1] := DrawFromBag;
  UpdatePreview;

  SpawnPiece;

  UpdateStats; // Neue Prozedur zum Aktualisieren der Labels
  FPieceDropDelay := PieceDropDelays[FLevel];
end;

procedure TForm1.CreateGorillaScene;
var
  leftBorder, rightBorder, backPlane: TGorillaCube;
  borderMat, backMat: TGorillaDefaultMaterialSource;
  borderThickness, backDepth: Single;
  i: Integer;
begin
  // Create a dummy as camera target at the board center
  FTarget := TDummy.Create(FGorilla);
  FTarget.Parent := FGorilla;
  FTarget.Position.Point := Point3D(COLS/2, ROWS/2, 0); // forward declare via consts below

  // Create a Gorilla camera as child of target (orbit-style) and link to viewport
  FCamera := TGorillaCamera.Create(FTarget);
  FCamera.Parent := FTarget;
{$IFDEF ANDROID}
  FCamera.Position.Point := Point3D(0, 0, 45); // offset back on Z
{$ELSE}
  FCamera.Position.Point := Point3D(0, 0, 28); // offset back on Z
{$ENDIF}
  FCamera.FOV := 45;
  FCamera.Target := FTarget;    // << link target so it always looks at board center

  FGorilla.Camera := FCamera;   // << link camera to viewport
  FGorilla.UsingDesignCamera := false;

  // Merke die Originalposition der Kamera für Shake-Restore
  FCameraOrigPos := FCamera.Position.Point;

  FLight := TGorillaLight.Create(FCamera);
  FLight.Parent := FCamera;
  FLight.LightType := TLightType.Point;

  var LSkyBox := TGorillaSkyBox.Create(FGorilla);
  LSkyBox.Parent := FGorilla;

{$IFDEF SKYBOX}
  LSkyBox.Size := Point3D(50, 50, 50);
  LSkyBox.Mode := TGorillaSkyBoxMode.CubeMapSkyBox;
  LSkyBox.FrontSide.LoadFromFile(FAssetsPath + 'SkyBox.png');
{$ELSE}
  LSkyBox.Mode := TGorillaSkyBoxMode.Blank;
  LSkyBox.Diffuse := TAlphaColorRec.Black;
{$ENDIF}

{$IFnDEF SKYBOX}
  // There is a bug on loading cubemaps on Android: Until the bug is fixed, we
  // setup a plane as background instead.
  var LPlane := TGorillaPlane.Create(FGorilla);
  LPlane.Parent := FGorilla;
  LPlane.SetSize(50, 50, 1);
  LPlane.Position.Point := Point3D(0, 0, -25);

  var LPlaneMat := TGorillaDefaultMaterialSource.Create(LPlane);
  LPlaneMat.Parent := LPlane;
  LPlaneMat.UseTexture0 := true;
  LPlaneMat.Texture.LoadFromFile(FAssetsPath + 'Skybox.jpg');
  LPlane.MaterialSource := LPlaneMat;
{$ENDIF}

  var LFloor := TGorillaPlane.Create(FGorilla);
  LFloor.Parent := FGorilla;
  LFloor.RotationAngle.X := 90;
  LFloor.SetSize(50, 50, 1);
  LFloor.Position.Point := Point3D(10, 22.5, 10);

  // Ein Untergrund mit bewegender Textur, die stets durchläuft
  var LFloorMat := TGorillaLambertMaterialSource.Create(LFloor);
  LFloorMat.Parent := LFloor;
  LFloorMat.UseTexture0 := true;
  LFloorMat.Texture.LoadFromFile(FAssetsPath + 'Floor.jpg');

  LFloorMat.MeasureTime := true;
  var LStr := TStringList.Create();
  try
    LStr.Text :=
      '''
      void SurfaceShader(inout TLocals DATA){
        vec2 l_TexOfs = DATA.TexCoord0.xy;

        float iTime = mod(mod(_TimeInfo.y, 360.0) * 0.2, 0.5);
        l_TexOfs += vec2(0.0, iTime);
        DATA.BaseColor.rgb = tex2D(_Texture0, l_TexOfs).rgb;
      }
      ''';
    LFloorMat.SurfaceShader := LStr;
  finally
    FreeAndNil(LStr);
  end;

  LFloor.MaterialSource := LFloorMat;

  // --- Visual borders / background for board size ---
  borderMat := TGorillaLambertMaterialSource.Create(FGorilla);
  borderMat.Parent := FGorilla;
  borderMat.Diffuse := $ff094e86; // dunkel halbtransparent
  borderMat.Emissive := $00000000;
  borderMat.UseTexture0 := false;

  backMat := TGorillaLambertMaterialSource.Create(FGorilla);
  backMat.Parent := FGorilla;
  backMat.Diffuse := $ff094e86; // halbtransparent bläulich
  backMat.Emissive := $00000000;
  backMat.UseTexture0 := false;

  borderThickness := 0.25;
  backDepth := 0.1;

  leftBorder := TGorillaCube.Create(FGorilla);
  leftBorder.Parent := FGorilla;
  leftBorder.SetSize(borderThickness, ROWS + 0.5, 1.5);
  leftBorder.Position.Point := Point3D(-0.5, (ROWS / 2) - 0.5, 0);
  leftBorder.MaterialSource := borderMat;
  leftBorder.SetOpacityValue(0.6);
  leftBorder.SetHitTestValue(false);
  leftBorder.Visible := True;

  rightBorder := TGorillaCube.Create(FGorilla);
  rightBorder.Parent := FGorilla;
  rightBorder.SetSize(borderThickness, ROWS + 0.5, 1.5);
  rightBorder.Position.Point := Point3D(COLS - 0.5, (ROWS / 2) - 0.5, 0);
  rightBorder.MaterialSource := borderMat;
  rightBorder.SetOpacityValue(0.6);
  rightBorder.SetHitTestValue(false);
  rightBorder.Visible := True;

  backPlane := TGorillaCube.Create(FGorilla);
  backPlane.Parent := FGorilla;
  backPlane.SetSize(COLS + 2, ROWS + 2, backDepth);
  backPlane.Position.Point := Point3D((COLS / 2) - 0.5, (ROWS / 2) - 0.5, -1);
  backPlane.MaterialSource := backMat;
  backPlane.SetOpacityValue(0.45);
  backPlane.SetHitTestValue(false);
  backPlane.Visible := True;

  // Create active & ghost cubes
  for i := 0 to 3 do
  begin
    FActiveCubes[i] := TGorillaCube.Create(FGorilla);
    FActiveCubes[i].Parent := FGorilla;
    FActiveCubes[i].SetSize(0.98,0.98,0.98);
    FActiveCubes[i].Visible := False;
    FActiveCubes[i].SetHitTestValue(false);
  end;

  for i := 0 to 3 do
  begin
    FGhostCubes[i] := TGorillaCube.Create(FGorilla);
    FGhostCubes[i].Parent := FGorilla;
    FGhostCubes[i].SetSize(0.98,0.98,0.98);
    FGhostCubes[i].Visible := False;
    FGhostCubes[i].SetOpacityValue(0.5);
    FGhostCubes[i].SetHitTestValue(false);

    var mat := TGorillaBlinnMaterialSource.Create(FGorilla);
    mat.Parent := FGorilla;
    mat.Diffuse := $FF888888;
    mat.Emissive := 0;
    mat.UseTexture0 := false;

    FGhostCubes[i].MaterialSource := mat;
  end;

  // --- Preview für die nächsten 2 Stücke ---
  // Positioniert die Vorschau rechts außerhalb des Boards
  for i := 0 to 1 do
  begin
    for var j := 0 to 3 do
    begin
      FPreviewCubes[i,j] := TGorillaCube.Create(FGorilla);
      FPreviewCubes[i,j].Parent := FGorilla;
      FPreviewCubes[i,j].SetSize(0.5, 0.5, 0.5); // etwas kleiner als Spielwürfel
      FPreviewCubes[i,j].Visible := False;

      // eigenes Material so dass die Vorschau dezenter ist
      var pMat := TGorillaDefaultMaterialSource.Create(FGorilla);
      pMat.Parent := FGorilla;
      pMat.ShadingModel := TGorillaShadingModel.smBlinnPhong;
      pMat.UseLighting := true;
      pMat.UseSpecular := true;
      pMat.Diffuse := $FF5D41DF;
      pMat.Emissive := $00000000;

      FPreviewCubes[i,j].MaterialSource := pMat;
      FPreviewCubes[i,j].SetOpacityValue(0.85);
      FPreviewCubes[i,j].SetHitTestValue(false);
    end;
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
// Animation der explodierenden Würfel / Vibration -> Explosion
const
  VIBRATE_TIME = 0.4; // Sekunden Vibration bevor Explosion
  VIBRATE_AMPLITUDE = 0.15; // maximale Verschiebung in Einheiten (relativ)
  VIBRATE_FREQ = 50.0; // Frequenz der Vibration (Hz)
  EXPLOSION_DURATION = 0.65; // Gesamtdauer nach Explosion (wird weiter unten verwendet)
var
  vibrOff: TPoint3D;
  phase: Single;
var
  i: Integer;
  pos: TVector3D;
  x,y: Integer;
  amplitude: Single;
begin
  // Wenn GameOver gesetzt ist, keine Spiel-Logik mehr (aber noch Animation beenden lassen)
  if FGameOver and not FIsAnimating then
    Exit;

  // Wenn eine Animation läuft, nur diese verarbeiten
  if FIsAnimating then
  begin
  {$IFDEF AUDIO}
    // Free the previously loaded item
    var LPrevItem := FAudioPlayer.GetSoundItemByFilename(FAssetsPath + 'row_clear.wav');
    if Assigned(LPrevItem) then
      FAudioPlayer.Sounds.Delete(LPrevItem.Index);

    var LItem := FAudioPlayer.LoadSoundItemFromFile(FAssetsPath + 'row_clear.wav');
    LItem.Play;
  {$ENDIF}

    FAnimationTimer := FAnimationTimer + DT;
    amplitude := VIBRATE_AMPLITUDE * abs(sin(FAnimationTimer) * 2);

    // Animation der explodierenden Würfel / Vibration -> Explosion
    for i := 0 to High(FAnimatedCubes) do
    begin
      if Assigned(FAnimatedCubes[i]) then
      begin
        // Wenn wir noch in der Vibrationsphase sind, wackeln die Würfel an Ort und Stelle
        if FAnimationTimer < VIBRATE_TIME then
        begin
          phase := FAnimatedPhases[i];
          // Sinusbasierte Vibration entlang einer kleinen zufälligen Richtung
          vibrOff := Point3D(
            FAnimatedVibrateDirs[i].X * Sin((FAnimationTimer * VIBRATE_FREQ) + phase) * amplitude,
            FAnimatedVibrateDirs[i].Y * Sin((FAnimationTimer * VIBRATE_FREQ) + phase) * amplitude,
            FAnimatedVibrateDirs[i].Z * Sin((FAnimationTimer * VIBRATE_FREQ) + phase) * amplitude
          );
          FAnimatedCubes[i].Position.Point := Point3D(
            FAnimatedBasePos[i].X + vibrOff.X,
            FAnimatedBasePos[i].Y + vibrOff.Y,
            FAnimatedBasePos[i].Z + vibrOff.Z
          );

          // leichte pulsierende Emissive / Scale kann hinzugefügt werden, falls gewünscht
          Continue;
        end;

        // Sobald die Vibrationsphase vorbei ist, löse einmalig die Explosion aus
        if (not FExplosionTriggered) then
        begin
          // Erzeuge für jeden animierten Würfel eine zufällige Explosionsgeschwindigkeit
          for var j := 0 to High(FAnimatedVelocities) do
          begin
            // Nur für vorhandene Würfel setzen
            if Assigned(FAnimatedCubes[j]) then
            begin
              FAnimatedVelocities[j] := Vector3D(
                (RandomRange(-400, 400) / 100) * (1 + RandomRange(0,200)/100), // X
                (RandomRange(-100, 300) / 100) * (1 + RandomRange(0,200)/100), // Y - leicht nach oben bevorzugt
                (RandomRange(-400, 400) / 100) * (1 + RandomRange(0,200)/100)  // Z
              );
            end;
          end;
          FExplosionTriggered := True;

        {$IFDEF AUDIO}
//          var LItem := FAudioPlayer.LoadSoundItemFromFile(FAssetsPath + 'explode.wav');
//          if Assigned(LItem) then LItem.Play;
        {$ENDIF}
        end;

        // Normale Bewegungsintegration nach Explosion
        pos := FAnimatedCubes[i].Position.Point;
        pos.X := pos.X + FAnimatedVelocities[i].X * DT * 5;
        pos.Y := pos.Y + FAnimatedVelocities[i].Y * DT * 5;
        pos.Z := pos.Z + FAnimatedVelocities[i].Z * DT * 5;
        FAnimatedCubes[i].Position.Point := pos;

        // Skalierung verringern, um das Verschwinden zu simulieren
        FAnimatedCubes[i].SetSize(Max(0.01, FAnimatedCubes[i].Scale.X - DT * 1.5),
                                 Max(0.01, FAnimatedCubes[i].Scale.Y - DT * 1.5),
                                 Max(0.01, FAnimatedCubes[i].Scale.Z - DT * 1.5));

        // Würfel langsam transparenter machen
        FAnimatedCubes[i].SetOpacityValue(Max(0, FAnimatedCubes[i].Opacity - DT * 2));
      end;
    end;

    // Animation beenden und Spielfeld aktualisieren, wenn der Timer abläuft
    if FAnimationTimer >= EXPLOSION_DURATION then
    begin
      FIsAnimating := False;

      // 1) Freigeben der animierten Würfel
      for i := 0 to High(FAnimatedCubes) do
      begin
        if Assigned(FAnimatedCubes[i]) then
        begin
          try
            FAnimatedCubes[i].Free;
          except
          end;
          FAnimatedCubes[i] := nil;
        end;
      end;
      SetLength(FAnimatedCubes, 0);
      SetLength(FAnimatedVelocities, 0);

      StartDropAnimation;
    end;

    Exit; // Timer-Prozedur verlassen, um Gravitation zu überspringen
  end;

  // --- Shake-Animation verarbeiten (HardDrop) ---
  if FIsShaking then
  begin
  {$IFDEF AUDIO}
    // Free the previously loaded item
    var LPrevItem := FAudioPlayer.GetSoundItemByFilename(FAssetsPath + 'harddrop_thud.wav');
    if Assigned(LPrevItem) then
      FAudioPlayer.Sounds.Delete(LPrevItem.Index);

    var LItem := FAudioPlayer.LoadSoundItemFromFile(FAssetsPath + 'harddrop_thud.wav');
    LItem.Play;
  {$ENDIF}

    FShakeTimer := FShakeTimer + DT;
    var t := FShakeTimer / FShakeDuration;
    if t > 1 then t := 1;

    // Dämpfungsfaktor: sanftes Abklingen (benutze quadratische Dämpfung für weicheres Ende)
    var damp := (1 - t) * (1 - t); // quadratisch -> langsameres Ausklingen

    // Für mehrere Hin-/Her-Schwingungen verwenden wir:
    // angle = 2*pi * Oscillations * t * baseFreqMultiplier
    // wobei baseFreq pro-Würfel variiert (FShakeBaseFreqs)
    for i := 0 to High(FShakingCubes) do
    begin
      if Assigned(FShakingCubes[i]) then
      begin
        var orig := FShakeOrigPos[i];
        var amp := FShakeAmplitudes[i];
        var bf := FShakeBaseFreqs[i];

        // Berechne Winkelfortschritt: 2*pi * Osc * (t) * baseFreq
        // t ist 0..1 über die ganze Dauer -> so entstehen exakt FShakeOscillations Zyklen
        var angleX := 2 * Pi * (FShakeOscillations * t) * bf.X;
        var angleY := 2 * Pi * (FShakeOscillations * t) * bf.Y;
        var angleZ := 2 * Pi * (FShakeOscillations * t) * bf.Z;

        // Sinus-basiertes Hin/Her, mit Dämpfung multipliziert
        var ox := Sin(angleX) * amp.X * damp;
        var oy := Sin(angleY) * amp.Y * damp;
        var oz := Sin(angleZ) * amp.Z * damp;

        FShakingCubes[i].Position.Point := Point3D(orig.X + ox, orig.Y + oy, orig.Z + oz);
      end;
    end;

    // --- Kamera synchron zum Block-Shake bewegen ---
    if FCameraShakeEnabled and Assigned(FCamera) then
    begin
      // Wir verwenden denselben t (0..1) und damp-Faktor wie bei den Würfeln
      // angle = 2*pi * Oscillations * t * baseFreq
      var camAngleX := 2 * Pi * (FShakeOscillations * t) * FCameraShakeBaseFreq.X;
      var camAngleY := 2 * Pi * (FShakeOscillations * t) * FCameraShakeBaseFreq.Y;
      var camAngleZ := 2 * Pi * (FShakeOscillations * t) * FCameraShakeBaseFreq.Z;

      // Sinus-basiertes Offset
      var camOx := Sin(camAngleX) * FCameraShakeAmplitude.X * damp;
      var camOy := Sin(camAngleY) * FCameraShakeAmplitude.Y * damp;
      var camOz := Sin(camAngleZ) * FCameraShakeAmplitude.Z * damp;

      // Setze die Kamera-Position relativ zur Originalposition
      FCamera.Position.Point := Point3D(FCameraOrigPos.X + camOx,
                                        FCameraOrigPos.Y + camOy,
                                        FCameraOrigPos.Z + camOz);
    end;

    // Ende der Animation: Positionen wiederherstellen und Lines löschen/weiter
    if FShakeTimer >= FShakeDuration then
    begin
      for i := 0 to High(FShakingCubes) do
      begin
        if Assigned(FShakingCubes[i]) then
        begin
          var p := FShakeOrigPos[i];
          FShakingCubes[i].Position.Point := Point3D(Round(p.X), Round(p.Y), Round(p.Z));
        end;
      end;

      // Aufräumen der Shake-Arrays
      SetLength(FShakingCubes, 0);
      SetLength(FShakeOrigPos, 0);
      SetLength(FShakeAmplitudes, 0);
      SetLength(FShakeBaseFreqs, 0);
      FIsShaking := False;
      FShakeTimer := 0;

      // Jetzt Lines prüfen / löschen (das war im LockPiece ursprünglich geplant)
      ClearLines;

      // Restore camera exact
      if FCameraShakeEnabled and Assigned(FCamera) then
      begin
        FCamera.Position.Point := Point3D(Round(FCameraOrigPos.X), Round(FCameraOrigPos.Y), Round(FCameraOrigPos.Z));
      end;
    end;

    Exit; // Während Shake keine andere Spiel-Logik ausführen
  end;

  // --- Drop-Animation nach Line Clear ---
  if FIsDropping then
  begin
    FDropTimer := FDropTimer + DT;
    var allDone := True;

    for i := 0 to High(FDroppingCubes) do
    begin
      var cube := FDroppingCubes[i];
      if not Assigned(cube) then Continue;

      var delay := FDropDelay[i];
      var localT := (FDropTimer - delay) / FDropDuration;

      if localT < 0 then
      begin
        allDone := False;
        Continue; // Würfel wartet noch
      end;

      if localT >= 1 then
        localT := 1
      else
        allDone := False;

      // Smooth easing (Quadratic In)
      var easedT := localT * localT;

      var startP := FDropStartPos[i];
      var endP := FDropEndPos[i];

      var newY := startP.Y + (endP.Y - startP.Y) * easedT;
      var newX := endP.X;
      var newZ := endP.Z;

      cube.Position.Point := Point3D(newX, newY, newZ);
    end;

    if allDone then
    begin
      // Aufräumen
      for i := 0 to High(FDroppingCubes) do
        if Assigned(FDroppingCubes[i]) then
          FDroppingCubes[i].Position.Point := FDropEndPos[i];

      SetLength(FDroppingCubes, 0);
      SetLength(FDropStartPos, 0);
      SetLength(FDropEndPos, 0);
      SetLength(FDropDelay, 0);

      FIsDropping := False;

      // 3) Aktualisieren + neues Stück (sofern nicht GameOver)
      RedrawBoard;
      UpdateStats;
      if not FGameOver then
        SpawnPiece;
    end;

    Exit; // während Drop keine normalen Gravitation/Inputs
  end;

  // --- Gravitations-Logik und DAS/ARR wie zuvor ---
  if FDownHeld or FHardDrop then
  begin
    if not TryMove(0,1) then
      LockPiece;
  end
  else
  begin
    FPieceDropTimer := FPieceDropTimer + DT;
    if FPieceDropTimer >= FPieceDropDelay then
    begin
      if not TryMove(0,1) then
        LockPiece;
      FPieceDropTimer := 0;
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
      FMoveIntensity := 1;
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

    if FWithGesture then
    begin
      // Because we're using gestures on Android, we need to reset our flags
      // if they were enabled in the gesture callback
      if FLeftHeld then
      begin
        FLeftHeld := False;
        FIsInitialMove := False;
        FMoveTimer := 0;
        FMoveIntensity := 1;
      end;

      if FRightHeld then
      begin
        FRightHeld := False;
        FIsInitialMove := False;
        FMoveTimer := 0;
        FMoveIntensity := 1;
      end;

      FDownHeld := False;
      FWithGesture := false;
    end;
  end;

{$IFDEF ANDROID}
  // Because Android rendering is event-based - we need to invalidate the scene
  // on each tick
  FGorilla.Invalidate();
{$ENDIF}
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Clear;

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

procedure TForm1.ViewportTap(Sender: TObject; const Point: TPointF);
var LVwPrtCtr : TPointF;
begin
  LVwPrtCtr := TPointF.Create(FGorilla.Width / 2, FGorilla.Height / 2);
  if Point.X < LVwPrtCtr.X then
  begin
    FRightHeld := True;
    FIsInitialMove := False;
    FMoveTimer := 0;
    FMoveIntensity := 1;
  end
  else if Point.X > LVwPrtCtr.X then
  begin
    FLeftHeld := True;
    FIsInitialMove := False;
    FMoveTimer := 0;
    FMoveIntensity := 1;
  end;

  FWithGesture := true;
end;

procedure TForm1.ViewportGesture(Sender: TObject;
  const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  if FGameOver then // Beispiel: Neustart mit Enter
  begin
    // Neustart-Flow: Spielfeld leeren, Status zurücksetzen, Timer ggf neu starten
    FGameOver := False;
    FScore := 0;
    FLevel := 0;
    FLinesCleared := 0;
    FPieceDropDelay := PieceDropDelays[0];
    FillChar(FBoard, SizeOf(FBoard), 0); // Spielfeld leeren
    RedrawBoard;
    UpdateStats;

    // Falls Timer gestoppt wurde, neu erstellen / starten
    if Assigned(FTimer) and not FTimer.Started then
    begin
      FTimer := TGorillaTimer.Create;
      FTimer.Interval := 16;
      FTimer.OnTimer := DoOnTick;
      FTimer.Start;
    end;

    SpawnPiece;
    Exit;
  end;

  if FHardDrop then
    Exit;

  FWithGesture := true;
  case EventInfo.GestureID of

    // Piece Movement
    sgiLeft  :
      begin
        FLeftHeld := True;
        FIsInitialMove := False;
        FMoveTimer := 0;
        FMoveIntensity := 10;
      end;

    sgiRight :
      begin
        FRightHeld := True;
        FIsInitialMove := False;
        FMoveTimer := 0;
        FMoveIntensity := 10;
      end;

    // Piece Rotation
    sgiUp :
      begin
        RotatePiece(FCurrentPiece, +1, FBoard);
        RedrawActive;
      end;

    sgiDown :
      begin
        RotatePiece(FCurrentPiece, -1, FBoard);
        RedrawActive;
      end;

    // Dropping
    igiLongTap :
      begin
        // Hard Drop
        FHardDrop := true;
        FHardDropExtraPoints := true;
      end;

    igiDoubleTap :
      begin
        // Hard Drop
        FHardDrop := true;
        FHardDropExtraPoints := true;
      end;
  end;
end;

procedure TForm1.InitBoard;
var x,y: Integer;
    cube: TGorillaCube;
    mat: TGorillaDefaultMaterialSource;
begin
  for y := 0 to ROWS-1 do
    for x := 0 to COLS-1 do
    begin
      FBoard[y,x] := ckNone;

      cube := TGorillaCube.Create(FGorilla);
      cube.Parent := FGorilla;
      cube.Position.Point := Point3D(x, y, 0);
      cube.SetSize(0.94,0.94,0.94);
      cube.SetHitTestValue(false);

      mat := TGorillaBlinnMaterialSource.Create(FGorilla);
      mat.ShadingModel := TGorillaShadingModel.smBlinnPhong;
      mat.UseLighting := true;
      mat.UseSpecular := true;
      mat.UseTexture0 := false;
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
          FCubes[y,x].SetHitTestValue(false);
        end;

        FCubes[y,x].Visible := True;
        mat := TGorillaDefaultMaterialSource(FCubes[y,x].MaterialSource);
        if not Assigned(mat) then
        begin
          mat := TGorillaBlinnMaterialSource.Create(FGorilla);
          mat.ShadingModel := TGorillaShadingModel.smBlinnPhong;
          mat.UseLighting := true;
          mat.UseSpecular := true;
          mat.UseTexture0 := false;
          mat.Parent := FGorilla;
          mat.Diffuse  := $FFFD1224;
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
  if FGameOver then Exit;

  // Nimm das nächste Piece aus der Vorschau (die Vorschau wurde aus der Bag gefüllt)
  FCurrentPiece.Kind := FNextPieces[0];

  // Schiebe die Queue: 1 -> 0, neue aus Bag in Slot 1
  FNextPieces[0] := FNextPieces[1];
  FNextPieces[1] := DrawFromBag;

  UpdatePreview;

  // restliche Initialisierung...
  FCurrentPiece.Matrix := PieceShapes[FCurrentPiece.Kind];
  FCurrentPiece.Rot := 0;
  FCurrentPiece.X := (COLS div 2) - 2;
  FCurrentPiece.Y := -2;

  if Collides(FCurrentPiece.Matrix, FCurrentPiece.X, FCurrentPiece.Y, FBoard) then
  begin
    HandleGameOver;
    Exit;
  end;

  RedrawActive;
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
    // Neustart-Flow: Spielfeld leeren, Status zurücksetzen, Timer ggf neu starten
    FGameOver := False;
    FScore := 0;
    FLevel := 0;
    FLinesCleared := 0;
    FPieceDropDelay := PieceDropDelays[0];
    FillChar(FBoard, SizeOf(FBoard), 0); // Spielfeld leeren
    RedrawBoard;
    UpdateStats;

    // Falls Timer gestoppt wurde, neu erstellen / starten
    if Assigned(FTimer) and not FTimer.Started then
    begin
      FTimer := TGorillaTimer.Create;
      FTimer.Interval := 16;
      FTimer.OnTimer := DoOnTick;
      FTimer.Start;
    end;

    SpawnPiece;
    Exit;
  end;

  if FHardDrop then
    Exit;

  case Key of
    vkLeft :
      begin
        FLeftHeld := True;
        FIsInitialMove := False;
        FMoveTimer := 0;
        FMoveIntensity := 1;
      end;

    vkRight :
      begin
        FRightHeld := True;
        FIsInitialMove := False;
        FMoveTimer := 0;
        FMoveIntensity := 1;
      end;

    vkDown : FDownHeld := True;

    vkUp :
      begin
        RotatePiece(FCurrentPiece, +1, FBoard);
        RedrawActive;
      end;

    vkZ :
      begin
        RotatePiece(FCurrentPiece, -1, FBoard);
        RedrawActive;
      end;

    0,
    vkSpace :
      begin
        FHardDrop := true;
        FHardDropExtraPoints := true;
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
        FMoveIntensity := 1;
      end;
    vkRight:
      begin
        FRightHeld := False;
        FIsInitialMove := False;
        FMoveTimer := 0;
        FMoveIntensity := 1;
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
    if FActiveCubes[i] <> nil then
    begin
      FActiveCubes[i].Visible := True;
      FActiveCubes[i].Position.Point :=
        Point3D(FCurrentPiece.X + p.X, (FCurrentPiece.Y + p.Y), 0);

      if not Assigned(FActiveCubes[i].MaterialSource) then
      begin
        mat := TGorillaBlinnMaterialSource.Create(FGorilla);
        mat.ShadingModel := TGorillaShadingModel.smBlinnPhong;
        mat.UseLighting := true;
        mat.UseSpecular := true;
        mat.UseTexture0 := false;
        mat.Parent := FGorilla;
        mat.Diffuse  := $FF0D1224;
        mat.Emissive := (col and $00FFFFFF) or ActivePieceEmission;
        FActiveCubes[i].MaterialSource := mat;
      end
      else
      begin
        mat := TGorillaDefaultMaterialSource(FActiveCubes[i].MaterialSource);
        mat.Emissive := (col and $00FFFFFF) or ActivePieceEmission;
      end;
    end;
  end;

  RedrawGhost;
end;

procedure TForm1.LockPiece;
var cells: TArray<TPoint>;
    p: TPoint;
    i: Integer;
    WasHardDrop: Boolean;
begin
  WasHardDrop := FHardDrop; // merken, ob der Lock durch HardDrop initiiert wurde
  FHardDrop := False;

  cells := CellsOf(FCurrentPiece.Matrix);
  for i:=0 to 3 do
  begin
    p := cells[i];
    if FCurrentPiece.Y + p.Y >= 0 then
      FBoard[FCurrentPiece.Y + p.Y, FCurrentPiece.X + p.X] := FCurrentPiece.Kind;
  end;

  // Aktive Würfel verstecken bis zum nächsten Teil
  for i:=0 to 3 do if Assigned(FActiveCubes[i]) then
    FActiveCubes[i].Visible := False;

  // Ghost-Würfel verstecken
  for i := 0 to 3 do if Assigned(FGhostCubes[i]) then
    FGhostCubes[i].Visible := False;

  // Board neu zeichnen (damit FCubes existieren / sichtbar sind)
  RedrawBoard;

  // Wenn es ein HardDrop war => Shake-Animation starten und ClearLines nach Ende der Animation ausführen.
  if WasHardDrop then
  begin
    StartShakeBoard;
  end
  else
  begin
    // normales Verhalten: sofort Linien prüfen / löschen
    ClearLines;
  end;
end;

procedure TForm1.Clear;
begin
  System.SetLength(FAnimatedCubes, 0);
  System.SetLength(FShakingCubes, 0);
  System.SetLength(FDroppingCubes, 0);

  for var I := 0 to 1 do
    for var J := 0 to 3 do
      FPreviewCubes[I][J] := nil;

  for var I := 0 to 3 do
    FActiveCubes [I] := nil;
  for var I := 0 to 3 do
    FGhostCubes [I] := nil;
end;

procedure TForm1.ClearLines;
var
  y, x: Integer;
  full: Boolean;
  linesToClear: TArray<Integer>;
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
    var LScore := 0;
    case Length(linesToClear) of
      1: begin
            LScore := 40 * (FLevel + 1);
            if FHardDropExtraPoints then
              LScore := LScore + 10;
         end;
      2: begin
            LScore := 100 * (FLevel + 1);
            if FHardDropExtraPoints then
              LScore := LScore + 50;
         end;
      3: begin
            LScore := 300 * (FLevel + 1);
            if FHardDropExtraPoints then
              LScore := LScore + 100;
         end;
      4: begin
            LScore := 1200 * (FLevel + 1);
            if FHardDropExtraPoints then
              LScore := LScore + 300;
         end;
    end;

    FScore := FScore + LScore;

    FLinesCleared := FLinesCleared + Length(linesToClear);
    if FLinesCleared div 10 > FLevel then
    begin
      Inc(FLevel);
      if FLevel <= High(PieceDropDelays) then
        FPieceDropDelay := PieceDropDelays[FLevel];
    end;

    // Speichere die zu löschenden Zeilen und starte Animation
    FRemovedLines := Copy(linesToClear);
    StartLineClearAnimation(linesToClear);
  end
  else
  begin
    // Wenn keine Zeilen gelöscht wurden, sofort das nächste Teil spawnen
    if not FGameOver then
      SpawnPiece;
  end;

  FHardDropExtraPoints := false;
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
    if FGhostCubes[i] <> nil then
    begin
      FGhostCubes[i].Visible := True;
      FGhostCubes[i].Position.Point := Point3D(GhostPiece.X + p.X, (GhostPiece.Y + p.Y), 0);
    end;
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
  FExplosionTriggered := False;

  // Blöcke aus den zu löschenden Reihen in das Animations-Array verschieben
  SetLength(FAnimatedCubes, Length(Lines) * COLS);
  SetLength(FAnimatedVelocities, Length(Lines) * COLS);
  SetLength(FAnimatedBasePos, Length(Lines) * COLS);
  SetLength(FAnimatedVibrateDirs, Length(Lines) * COLS);
  SetLength(FAnimatedPhases, Length(Lines) * COLS);

  idx := 0;
  for y in Lines do
  begin
    for x := 0 to COLS - 1 do
    begin
      cube := FCubes[y, x];
      if Assigned(cube) and cube.Visible then
      begin
        // Basisposition speichern (für Vibration)
        FAnimatedBasePos[idx] := cube.Position.Point;

        // Anfang: keine Explosionsgeschwindigkeit, nur Vibration
        FAnimatedVelocities[idx] := Vector3D(0,0,0);

        // Zufällige Vibrationsrichtung und Phase
        var dir := Vector3D(
          RandomRange(-100, 100) / 100,
          RandomRange(-100, 100) / 100,
          RandomRange(-100, 100) / 100
        );
        // Normieren, falls nicht null
        var len := Sqrt(dir.X*dir.X + dir.Y*dir.Y + dir.Z*dir.Z);
        if len > 0 then
          dir := Vector3D(dir.X/len, dir.Y/len, dir.Z/len)
        else
          dir := Vector3D(0,1,0);
        FAnimatedVibrateDirs[idx] := dir;

        FAnimatedPhases[idx] := RandomRange(0, 31415) / 5000; // 0..~6.283 (radians)

        // Referenz in das Animationsarray übernehmen
        FAnimatedCubes[idx] := cube;

        Inc(idx);
      end;

      // Referenz im Grid entfernen – das Objekt gehört nun der Animation
      FCubes[y,x] := nil;
    end;
  end;

  SetLength(FAnimatedCubes, idx);
  SetLength(FAnimatedVelocities, idx);
  SetLength(FAnimatedBasePos, idx);
  SetLength(FAnimatedVibrateDirs, idx);
  SetLength(FAnimatedPhases, idx);
end;

procedure TForm1.HandleGameOver;
begin
  if FGameOver then Exit;

  for var s := 0 to 1 do
    for var c := 0 to 3 do
      if Assigned(FPreviewCubes[s,c]) then
        FPreviewCubes[s,c].Visible := False;

  // Queue UI-Aufrufe in Main-Thread
  TThread.Queue(nil,
    procedure
    var i: Integer;
    begin
      for i := 0 to 3 do
        if Assigned(FActiveCubes[i]) then
          try
            FActiveCubes[i].Visible := False;
          except
          end;

      // Timer beenden, damit keine DoOnTick-Aufrufe mehr kommen
      if Assigned(FTimer) and FTimer.Started then
        FTimer.Terminate;

      FinalScoreLabel.Text := IntToStr(FScore);
      Image4.Visible := true;

      Clear;
    end);
end;

procedure TForm1.UpdatePreview;
var
  slot, k: Integer;
  cells: TArray<TPoint>;
  p: TPoint;
  baseX, baseY: Single;
  col: TAlphaColor;
  mat: TGorillaDefaultMaterialSource;
begin
  // Preview-Slots rechts neben dem Feld platzieren (außerhalb der rechten Border)
  // Slot 0 oben, Slot 1 darunter
  for slot := 0 to 1 do
  begin
    // Basis-Position für diesen Slot (anpassen wenn nötig)
    baseX := COLS + 3 + (slot * 0); // gleiche X-Position; Y unterscheidet sich weiter unten
    baseY := 18 + slot * 2; // Abstand zwischen Slot 0 und 1

    // Berechne Zell-Offsets für das Piece (verwende die PieceMatrix)
    cells := CellsOf(PieceShapes[FNextPieces[slot]]);

    col := PieceColors[FNextPieces[slot]];
    for k := 0 to 3 do
    begin
      p := cells[k];
      // Positionierung: wir zentrieren jede 4x4-Matrix innerhalb eines kleinen Vorschau-Box
      // Verschiebe X und Y so, dass es gut neben dem Grid steht
      FPreviewCubes[slot,k].Visible := True;
      FPreviewCubes[slot,k].Position.Point :=
        Point3D(baseX + (p.X * 0.5 - 1.5), baseY + (p.Y * 0.5 - 1.0), 0);

      // Materialfarbe / Emissive wie beim normalen Block (etwas weniger intensiv möglich)
      mat := TGorillaDefaultMaterialSource(FPreviewCubes[slot,k].MaterialSource);
      if Assigned(mat) then
  //      mat.Emissive := (col and $00FFFFFF) or ($33000000) // leicht gedämpft
      else
      begin
        mat := TGorillaDefaultMaterialSource.Create(FGorilla);
        mat.ShadingModel := TGorillaShadingModel.smBlinnPhong;
        mat.UseLighting := true;
        mat.UseSpecular := true;
        mat.Parent := FGorilla;
//        mat.Emissive := (col and $00FFFFFF) or ($33000000);
        FPreviewCubes[slot,k].MaterialSource := mat;
      end;
    end;
  end;
end;

procedure TForm1.ShuffleBag;
var
  i, j: Integer;
  tmp: TCellKind;
begin
  // Fisher-Yates Shuffle für die 7 Elemente
  for i := 6 downto 1 do
  begin
    j := Random(i + 1); // 0..i
    tmp := FBag[i];
    FBag[i] := FBag[j];
    FBag[j] := tmp;
  end;
end;

procedure TForm1.RefillBag;
var
  i: Integer;
begin
  // Fülle Bag mit den 7 Tetromino-Typen (ckI..ckL)
  for i := 0 to 6 do
    FBag[i] := TCellKind(Ord(ckI) + i);
  // Mische
  ShuffleBag;
  FBagIndex := 0;
end;

function TForm1.DrawFromBag: TCellKind;
begin
  // Wenn Bag leer (Index > 6), refill
  if FBagIndex > 6 then
    RefillBag;

  Result := FBag[FBagIndex];
  Inc(FBagIndex);

  // Wenn wir gerade das letzte Element herausgenommen haben,
  // bereite sofort eine neue Bag vor (optional)
  if FBagIndex > 6 then
    RefillBag;
end;

procedure TForm1.StartShakeBoard;
var
  y, x, idx: Integer;
  cube: TGorillaCube;
  amp, baseFreq: TVector3D;
begin
  // Parameter: Dauer und Anzahl der kompletten Schwingungen (Hin+Her = 1 Oscillation)
  FShakeDuration := 0.5;      // Gesamtdauer in Sekunden (länger, damit mehrere Schwingungen sichtbar sind)
  FShakeOscillations := 4;    // Anzahl voller Hin-/Her-Schwingungen
  FShakeTimer := 0;
  FIsShaking := True;

  // Sammle alle sichtbaren/abgelegten Würfel in ein Array
  SetLength(FShakingCubes, 0);
  for y := 0 to ROWS - 1 do
    for x := 0 to COLS - 1 do
      if (FBoard[y,x] <> ckNone) and Assigned(FCubes[y,x]) and FCubes[y,x].Visible then
      begin
        idx := Length(FShakingCubes);
        SetLength(FShakingCubes, idx + 1);
        FShakingCubes[idx] := FCubes[y,x];
      end;

  // Reserve helper arrays
  SetLength(FShakeOrigPos, Length(FShakingCubes));
  SetLength(FShakeAmplitudes, Length(FShakingCubes));
  SetLength(FShakeBaseFreqs, Length(FShakingCubes));

  // Fill start values: origin, amplitude, base frequency
  for idx := 0 to High(FShakingCubes) do
  begin
    cube := FShakingCubes[idx];
    if Assigned(cube) then
    begin
      // original position merken
      FShakeOrigPos[idx] := cube.Position.Point;

      // amplituden: etwas zufällig, ggf. vom Benutzer hochskaliert (du hast *3 verwendet)
      amp := Vector3D(
        (RandomRange(8, 36) / 100.0),  // X-Amplitude (0.08..0.36)
        (RandomRange(4, 18) / 100.0),  // Y-Amplitude
        (RandomRange(0, 12) / 100.0)   // Z-Amplitude
      );
      // Optional: hier die Werte um 3 multiplizieren, wenn du es extra stark willst:
      // amp := Vector3D(amp.X * 3, amp.Y * 3, amp.Z * 3);
      FShakeAmplitudes[idx] := amp;

      // Basisfrequenz in Hz (wird zusammen mit FShakeOscillations verwendet)
      // Wir wählen moderate Basen, Variation pro Würfel
      baseFreq := Vector3D(
        RandomRange(8, 14) / 10.0, // 0.8..1.4
        RandomRange(8, 14) / 10.0,
        RandomRange(8, 14) / 10.0
      );
      // Wenn du bereits überall *3 multipliziert hast, das ergibt stärkere, schnellere Bewegung.
      // baseFreq := baseFreq * 3; // falls gewünscht (du hattest x3)
      FShakeBaseFreqs[idx] := baseFreq;
    end;
  end;

  // Optionale Kameravariation pro-Shake: leichte Zufallskomponenten
  if FCameraShakeEnabled and Assigned(FCamera) then
  begin
    // Merke die Originalposition (erneut, falls sie sich zwischen Spawns verändert hat)
    FCameraOrigPos := FCamera.Position.Point;

    // Optional: leichte zufällige Skalierung der Kamera-Amplitude (Variation)
    FCameraShakeAmplitude := Vector3D(
      FCameraShakeAmplitude.X * (0.9 + RandomRange(0,20)/100),
      FCameraShakeAmplitude.Y * (0.9 + RandomRange(0,20)/100),
      FCameraShakeAmplitude.Z
    );

    // Optional: Basisfrequenzen leicht variieren
    FCameraShakeBaseFreq := Vector3D(
      FCameraShakeBaseFreq.X * (0.9 + RandomRange(0,20)/100),
      FCameraShakeBaseFreq.Y * (0.9 + RandomRange(0,20)/100),
      FCameraShakeBaseFreq.Z
    );
  end;
end;

procedure TForm1.StartDropAnimation;
var
  x, y, idx, dropCount: Integer;
  cube: TGorillaCube;
  startPos, endPos: TVector3D;
  removedFlag: array[0..ROWS-1] of Boolean;
begin
  if Length(FRemovedLines) = 0 then Exit;

  // 1) Markiere entfernte Linien
  FillChar(removedFlag, SizeOf(removedFlag), 0);
  for y in FRemovedLines do
    if (y >= 0) and (y < ROWS) then
      removedFlag[y] := True;

  // 2) Lösche die Linien sofort im Board
  for y in FRemovedLines do
    for x := 0 to COLS-1 do
    begin
      FBoard[y, x] := ckNone;
      if Assigned(FCubes[y, x]) then
      begin
        FCubes[y, x].Visible := False;
        FCubes[y, x] := nil;
      end;
    end;

  SetLength(FDroppingCubes, 0);
  SetLength(FDropStartPos, 0);
  SetLength(FDropEndPos, 0);
  SetLength(FDropDelay, 0);

  // 3) Drop nur für Würfel oberhalb gelöschter Linien
  for x := 0 to COLS - 1 do
  begin
    dropCount := 0;
    for y := ROWS - 1 downto 0 do
    begin
      if removedFlag[y] then
      begin
        Inc(dropCount);
        Continue;
      end;

      if FBoard[y, x] <> ckNone then
      begin
        cube := FCubes[y, x];
        if Assigned(cube) and (dropCount > 0) then
        begin
          startPos := cube.Position.Point;
          endPos := Point3D(startPos.X, startPos.Y + dropCount, startPos.Z); // nur fallen um dropCount

          idx := Length(FDroppingCubes);
          SetLength(FDroppingCubes, idx + 1);
          SetLength(FDropStartPos, idx + 1);
          SetLength(FDropEndPos, idx + 1);
          SetLength(FDropDelay, idx + 1);

          FDroppingCubes[idx] := cube;
          FDropStartPos[idx] := startPos;
          FDropEndPos[idx] := endPos;
          FDropDelay[idx] := Random * 0.18;

          // Board sofort aktualisieren
          FBoard[y + dropCount, x] := FBoard[y, x];
          FCubes[y + dropCount, x] := cube;

          FBoard[y, x] := ckNone;
          FCubes[y, x] := nil;
        end;
      end;
    end;
  end;

  if Length(FDroppingCubes) > 0 then
  begin
    FDropTimer := 0;
    FDropDuration := 0.35;
    FIsDropping := True;
  end;

  SetLength(FRemovedLines, 0);
end;

end.
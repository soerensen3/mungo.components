unit mungo.components.filebrowser;

{$mode objfpc}{$H+}
{$ModeSwitch nestedprocvars}

interface

uses
  Classes, SysUtils, Math,

  Generics.Collections,

  Graphics,
  Controls,
  ExtCtrls,

  FileUtil,

  mungo.components.colors,
  mungo.intf.FilePointer,
  mungo.intf.git,

  laz.VirtualTrees, LazFileUtils;

type
  PTreeData = ^TTreeData;
  TTreeData = record
    FileType: Integer;
    Name: String;
    FullName: String;
    GitStatus: TGitFileStatus;
  end;

  { TFile2TreeView }

  TFile2TreeView = class ( TFileSearcher )
    private
//      FLastLevel: Integer;
      FLastParent: PVirtualNode;
      FStringTree: TLazVirtualStringTree;
      Dict: specialize TDictionary < String, TGitFileStatus >;

    protected
      procedure DoFileFound; override;
      procedure DoDirectoryEnter; override;

      function AddNode( AParent: PVirtualNode; AType: Integer; AFileName: String ): PVirtualNode;

      property LastParent: PVirtualNode read FLastParent write FLastParent;
//      property LastLevel: Integer read FLastLevel write FLastLevel;
      property StringTree: TLazVirtualStringTree read FStringTree write FStringTree;
  end;

  { TVirtualStringTreeFileBrowser }

  TVirtualStringTreeFileBrowser = class ( TLazVirtualStringTree )
    private
      FSearchPaths: TFilePointerList;

      procedure FileTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
      procedure FileTreeGetImageIndex(Sender: TBaseVirtualTree;
        Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
        var Ghosted: Boolean; var ImageIndex: Integer);
      procedure FileTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
        Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
      procedure FileTreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
      procedure FileTreeBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
        CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);

    public

      procedure UpdateFolders;

      procedure AddSearchPath( AFileName: String );

      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;

      property SearchPaths: TFilePointerList read FSearchPaths write FSearchPaths;
  end;

  const //File type constants
    FT_FOLDER_ROOT  = 0;
    FT_FOLDER       = 1;
    FT_FILE_GENERIC = 2;

  procedure Register;



implementation

procedure Register;
begin
  RegisterComponents( 'mungo', [ TVirtualStringTreeFileBrowser ]);
end;

{ TVirtualStringTreeFileBrowser }

procedure TVirtualStringTreeFileBrowser.UpdateFolders;
var
  Files: TFile2TreeView;
  Location: TFilePointer;

  procedure StatusCallBack(AFileStatus: TGitStatusFileRecord);
  var
    FN: String;
  begin
    FN:= CreateAbsolutePath( AFileStatus.FileName, Location.FileName );
    if ( not Files.Dict.ContainsKey( FN )) then
      Files.Dict.Add( FN, AFileStatus.Status );
  end;

begin
  BeginUpdate;
  Clear;
  Files:= TFile2TreeView.Create;
  Files.StringTree:= Self;
  Files.Dict:= specialize TDictionary < String, TGitFileStatus >.Create;
  for Location in SearchPaths do begin
    GitStatus.GetRepoStatus( Location.FileName, @StatusCallBack );
    Files.LastParent:= Files.AddNode( nil, FT_FOLDER_ROOT, Location.FileName );
//    Files.LastParent^.NodeHeight:= Files.LastParent^.NodeHeight * 2;
    Files.Search( Location.FileName );
  end;
  FreeAndNil( Files.Dict );
  FreeAndNil( Files );
  EndUpdate;
end;

procedure TVirtualStringTreeFileBrowser.AddSearchPath(AFileName: String);
var
  P: TFilePointer;
begin
  P:= FilePointers.GetFilePointer( AFileName );
  SearchPaths.Add( P );
end;

procedure TVirtualStringTreeFileBrowser.FileTreeGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  Data: PTreeData;
begin
  Data:= GetNodeData( Node );
//  if ( Column = 0 ) then
    CellText:= Data^.Name;
end;

procedure TVirtualStringTreeFileBrowser.FileTreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
  var Result: Integer);
var
  Data1: PTreeData;
  Data2: PTreeData;
begin
  Data1:= GetNodeData( Node1 );
  Data2:= GetNodeData( Node2 );
  if ( Data1^.FileType = FT_FOLDER_ROOT ) then
    Result:= 0
  else if ( Data1^.FileType = Data2^.FileType ) then
    Result:= CompareStr( Data1^.Name, Data2^.Name )
  else
    Result:= CompareValue( Data1^.FileType, Data2^.FileType );
end;

procedure TVirtualStringTreeFileBrowser.FileTreeBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  Data: PTreeData;
begin
  Data:= GetNodeData( Node );
  if ( gfsIgnored in Data^.GitStatus ) then
    TargetCanvas.Brush.Color:= Gray50
  else if ( gfsUntracked in Data^.GitStatus ) then
    TargetCanvas.Brush.Color:= Blue50
  else if ([ gfsStagedAdded, gfsUnstagedAdded ] * Data^.GitStatus <> []) then
    TargetCanvas.Brush.Color:= Green50
  else if ([ gfsStagedModified, gfsUnstagedModified ] * Data^.GitStatus <> []) then
    TargetCanvas.Brush.Color:= Orange50
  else if ([ gfsStagedDeleted, gfsUnstagedDeleted ] * Data^.GitStatus <> []) then
    TargetCanvas.Brush.Color:= Red50
  else
    TargetCanvas.Brush.Color:= White;
  TargetCanvas.FillRect( CellRect );
end;

procedure TVirtualStringTreeFileBrowser.FileTreeGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  Data: PTreeData;
begin
  Data:= GetNodeData( Node );
  ImageIndex:= Data^.FileType;
end;

procedure TVirtualStringTreeFileBrowser.FileTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PTreeData;
begin
  Data:= GetNodeData( Node );
  Data^.FullName:= '';
  Data^.Name:= '';
end;

constructor TVirtualStringTreeFileBrowser.Create(AOwner: TComponent);
begin
  inherited Create( AOwner );

  SearchPaths:= TFilePointerList.Create( False );
  OnGetText:= @FileTreeGetText;
  OnGetImageIndex:= @FileTreeGetImageIndex;
  OnCompareNodes:=@FileTreeCompareNodes;
  OnBeforeCellPaint:=@FileTreeBeforeCellPaint;
  OnFreeNode:=@FileTreeFreeNode;
  TreeOptions.AutoOptions:= TreeOptions.AutoOptions + [ toAutoSort ];

  NodeDataSize:= SizeOf( TTreeData );
end;

destructor TVirtualStringTreeFileBrowser.Destroy;
begin
  FreeAndNil( FSearchPaths );
  inherited Destroy;
end;

{ TFile2TreeView }

procedure TFile2TreeView.DoFileFound;
begin
  AddNode( LastParent, FT_FILE_GENERIC, FileName );

  inherited DoFileFound;
end;

procedure TFile2TreeView.DoDirectoryEnter;
var
  CurDir: PVirtualNode;
begin
//  WriteLn( 'Enter: ', FileName );
  CurDir:= LastParent;
  while ( Assigned( CurDir ) and ( Level + 1 <= StringTree.GetNodeLevel( CurDir ))) do begin
    CurDir:= CurDir^.Parent;
  end;

  LastParent:= AddNode( CurDir, FT_FOLDER, FileName );
  inherited DoDirectoryEnter;
end;

function TFile2TreeView.AddNode(AParent: PVirtualNode; AType: Integer; AFileName: String): PVirtualNode;
var
  Data: PTreeData;
begin
  Result:= StringTree.AddChild( AParent );
  Data:= StringTree.GetNodeData( Result );
  Data^.FullName:= AFileName;
  Data^.Name:= ExtractFileName( AFileName );
  Data^.FileType:= AType;
  if ( Assigned( Dict )) then
    Dict.TryGetValue( AFileName, Data^.GitStatus );
end;



end.


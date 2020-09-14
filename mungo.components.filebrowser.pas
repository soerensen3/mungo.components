unit mungo.components.filebrowser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,

  Controls,
  ExtCtrls,

  FileUtil,

  mungo.intf.FilePointer,

  laz.VirtualTrees, LazFileUtils;

type
  PTreeData = ^TTreeData;
  TTreeData = record
    FileType: Integer;
    Name: String;
    FullName: String;
  end;

  { TFile2TreeView }

  TFile2TreeView = class ( TFileSearcher )
    private
//      FLastLevel: Integer;
      FLastParent: PVirtualNode;
      FStringTree: TLazVirtualStringTree;

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

    public
      procedure FileTreeGetImageIndex(Sender: TBaseVirtualTree;
        Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
        var Ghosted: Boolean; var ImageIndex: Integer);
      procedure FileTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
        Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);

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
begin
  BeginUpdate;
  Clear;
  Files:= TFile2TreeView.Create;
  Files.StringTree:= Self;
  for Location in SearchPaths do begin
    Files.LastParent:= Files.AddNode( nil, FT_FOLDER_ROOT, Location.FileName );
//    Files.LastParent^.NodeHeight:= Files.LastParent^.NodeHeight * 2;
    Files.Search( Location.FileName );
  end;
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

procedure TVirtualStringTreeFileBrowser.FileTreeGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  Data: PTreeData;
begin
  Data:= GetNodeData( Node );
  ImageIndex:= Data^.FileType;
end;

constructor TVirtualStringTreeFileBrowser.Create(AOwner: TComponent);
begin
  inherited Create( AOwner );

  SearchPaths:= TFilePointerList.Create( False );
  OnGetText:= @FileTreeGetText;
  OnGetImageIndex:= @FileTreeGetImageIndex;

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
  WriteLn( 'Enter: ', FileName );
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
end;



end.


#### 1. 前言

以前在 Delphi 里面判断一个文件是否存在：

If FileExists(MyFileName) then

到了 FireMonkey 支持多平台，Delphi 提供了一个新的单元：System.IoUtils，这个单元里的东西，在 Delphi支持的所有平台上，都能正确执行

这个单元包括三个主要的类：TPath, TDirectory, TFile

如果想获得系统路径，比如当前用户的【为的文档】的路径，用 TPath.GetDocumentsPath 函数；

如果想对某个具体的目录（文件夹）进行操作，比如复制整个文件夹，用：

TDirectory.Copy(SourceFolderName, DestFolderName);

如果想对文件进行操作，比如复制文件，删除问题，用 TFile.Copy(SourceFileName, DestFileName) 和 TFile.Delete(MyFileName)  这样的函数。














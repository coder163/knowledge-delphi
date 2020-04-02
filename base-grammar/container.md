

```pascal
// 声明

bplList: TDictionary<string, Integer>;

// 创建

bplList := TDictionary<string, Integer>.Create;

// 往字典里增加一条

h := LoadPackage(bplName);

bplList.Add(bplName, h);

// 查字典

if not bplList.ContainsKey(bplName) then

// 遍历字典

var
i: Integer;
......
for i in bplList.Values do
UnloadPackage(i);
// TryGetValue 
p: TPrintTaskThread;

if g_PrintTasks.TryGetValue(frmPos.cdsPrinter.FieldByName('prnName').Text, p) then
p.PrintQueue.Enqueue(OneTimePrint);
```
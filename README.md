tdata
=====

根据输入不同文件的不同内容格式,只需要编写少量的erl脚本,写好模板,就能转换生成想要的文件格式和内容

可以说扩展性很强,自由度很高,因此也需要一点学习成本

理论上按照现在的设计是支持无数的文件类型转换,但是目前默认自带的只有`excel`文件的转换模块,如果要转换其它输入格式,可以自己写`loader`

> 下面以默认的`excel_loader`为例子

使用
-----
具体请看[tdata_SUITE](test/ct/tdata_SUITE/tdata_SUITE.erl)文件

excel_loader
-----
### 功能
* skip_comments -> 跳过N行注释
* groups -> 支持合并单元格功能
* checks -> 支持每个单元格的检查和转换
* 支持每个sheet一个转换方法

### excel_loader配置使用
具体请看[tdata_excel_loader_SUITE](test/ct/tdata_excel_loader_SUITE/tdata_excel_loader_SUITE.erl)文件

build
-----
```erlang
    $ rebar3 compile
```

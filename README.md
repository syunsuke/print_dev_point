# print_dev_point

開発登録簿を基に調査した開発物件一覧表に
位置情報を付加したデータを準備すると、
マップ上に、その場所をプロットしてくれるルーチンです。

Webブラウザで表示すれば、そのままプロット図を印刷することも可能です。

## ファイルの準備

開発物件一覧表のエクセルシートの「N列」（１４番目の列）に
Google Mapで取得できる形式の緯度経度文字列を書き込みます。
そして、そのシートをCSVファイルとして保存します。

dataディレクトリにサンプルのCSVファイルがあるので、
自分でCSVファイルを準備する際の参考にしてください。

## 必要なライブラリ

- tidyverse
- leaflet


## 使い方

あとは、example_making_map.nb.htmlを参照してください。

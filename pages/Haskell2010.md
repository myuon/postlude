# Haskell2010 Language Report 日本語訳

Simon Marlow (editor)

myuon (翻訳)

著作権表示
著者と発行者はこのレポートをHaskellコミュニティ全体へ帰属させることを意図しており、この表示を含め文章全体が再現される限りにおいてはコピーと配布をいかなる目的でも許可している。このレポートの改変も、改変がそうと明示されており、Haskell2010言語の定義であることを主張しなければ、いかなる目的でもコピーや再配布することができる。

> Copyright notice.
> The authors and publisher intend this Report to belong to the entire Haskell community, and grant permission to copy and distribute it for any purpose, provided that it is reproduced in its entirety, including this Notice. Modified versions of this Report may also be copied and distributed for any purpose, provided that the modified version is clearly presented as such, and that it does not claim to be a definition of the language Haskell 2010.

- 目次
- 前書き
    - 目標
    - Haskell2010: 言語とライブラリ
    - Haskell98からの拡張
    - リソース
    - 言語の作成

### I. Haskell 2010 言語

1. 序説
    - プログラムの構造
    - Haskellのコア
    - 値と型
    - 名前空間
1. 字句構造
    - 慣習的表記
    - 字句の構造
    - コメント
    - 識別子と演算子
    - 数値リテラル
    - 文字と文字列リテラル
    - レイアウト
1. 式
    - エラー
    - 変数、コンストラクタ、演算子、リテラル
    - カリー化された適用とラムダ抽象
    - 演算子の適用
    - セクション
    - 条件
    - リスト
    - タプル
    - ユニット式と括弧つき式
    - 算術列
    - リスト内法表記
    - let式
    - case式
    - do式
    - フィールドラベルのついたデータ型
    - 型シグネチャ
    - パターンマッチ
1. 宣言と束縛
    - 型とクラスの概観
    - ユーザー定義データ型
    - 型クラスとオーバーロード
    - ネストされた宣言
    - 関数とパターン束縛の静的な意味論
    - カインド推論
1. モジュール
    - モジュール構造
    - exportリスト
    - import宣言
    - インスタンス宣言のimportとexport
    - 名前衝突とクロージャー
    - 標準Prelude
    - 分割コンパイル
    - 抽象データ型
1. 事前定義された型とクラス
    - 標準的なHaskellの型
    - 正格評価
    - 標準的なHaskellのクラス
    - 数値
1. 入出力の基本
    - 標準的なI/O関数
    - I/O操作列
    - I/Oモナドにおける例外処理
1. Foreign Function Interface
    - 外部言語
    - コンテキスト
    - 字句構造
    - 外部宣言
    - 外部エンティティの規格
    - マーシャリング
    - 外部Cインターフェイス
1. 標準Prelude
    - Prelude `PreludeList`
    - Prelude `PreludeText`
    - Prelude `PreludeIO`
1. 文法リファレンス
    - 慣習的表記
    - 字句文法
    - レイアウト
    - 文芸的コメント
    - 文脈自由文法
    - 結合の解決
1. インスタンス導出の規格
    - `Eq`と`Ord`のインスタンス導出
    - `Enum`のインスタンス導出
    - `Bounded`のインスタンス導出
    - `Read`と`Show`のインスタンス導出
    - 例
1. コンパイラプラグマ
    - インライン化
    - 限定化
    - 言語拡張

### Haskell 2010 ライブラリ

13. `Control.Monad`
    - ファンクターとモナドクラス
    - 関数
1. `Data.Array`
    - 不変非正格配列
    - 配列の構成
    - 配列へのアクセス
    - 配列の逐次更新
    - 導出された配列
    - 規格
1. `Data.Bits`
1. `Data.Char`
    - 文字と文字列
    - 文字の分類
    - 複合語の慣習
    - 単一の数値文字
    - 数値の表現
    - 文字列の表現
1. `Data.Complex`
    - 直行形式
    - 極形式
    - 共役
    - 規格
1. `Data.Int`
    - 符号付き整数型
1. `Data.Ix`
    - `Ix`クラス
    - `Ix`インスタンスの導出
1. `Data.List`
    - 基本的な関数
    - リスト変換
    - リストの縮小(畳み込み)
    - リストの作成
    - 部分リスト
    - リストの検索
    - リストの添え字アクセス
    - リストのzip/unzip
    - 特別なリスト
    - 一般化された関数
1. `Data.Maybe`
    - `Maybe`型と操作
    - 規格
1. `Data.Ratio`
    - 規格
1. `Data.Word`
    - 符号なし整数型
1. `Foreign`
1. `Foreign.C`
1. `Foreign.C.Error`
    - `errno`値のHaskellにおける表現
1. `Foreign.C.String`
    - Cの文字列
    - Cのワイド文字列
1. `Foreign.C.Types`
    - Cの型の表現
1. `Foreign.ForeignPtr`
    - ファイナライズされたデータポインター
1. `Foreign.Marshal`
1. `Foreign.Marshal.Alloc`
    - メモリ確保
1. `Foregin.Marshal.Array`
    - 配列のマーシャリング
1. `Foreign.Marshal.Error`
1. `Foreign.Marshal.Utils`
    - 一般的なマーシャリングのユーティリティ
1. `Foregin.Ptr`
    - データポインタ
    - 関数ポインタ
    - 整数型とポインタのロスのない相互変換
1. `Foregin.StablePtr`
    - Haskellの値への安定参照
1. `Foregin.Storable`
1. `Numeric`
    - 表示
    - 読み取り
    - その他
1. `System.Environment`
1. `Sytem.Exit`
1. `System.IO`
    - IOモナド
    - ファイルとハンドル
    - ファイルのオープンとクローズ
    - ハンドルに対する捜査
    - テキストの入力と出力
1. `System.IO.Error`
    - I/Oエラー
    - I/Oエラーの型
    - I/Oエラーの送出と捕捉

参考文献

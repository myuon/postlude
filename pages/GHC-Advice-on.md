# GHC利用テクニック

See: [9. Advice on: sooner, faster, smaller, thriftier](https://downloads.haskell.org/~ghc/master/users-guide/sooner.html)

## コンパイル高速化

- `-O`, `-O2`最適化をしない
    - GHCは最適化に多くの時間を使う
- メモリ(ヒープ)を増やす
    - GCの回数が減る
    - `-H<size>`によりヒープサイズを指定できる
- `Read`を使わない

## プログラム高速化

- `-O`, `-O2`最適化を行う
- LLVM経由でコンパイルを行う
    - ネイティブコードよりも早い場合がある(特に重い数値の計算で効果的である)
    - `-fllvm`フラグを指定する
- オーバーロードされた関数を使わない
    - `SPECIALIZE`する
- `-XStrict`拡張を使う
    - あるいは必要な箇所で`seq`, `deepseq`による強制的な評価を行う
- データ型のコンストラクタを1つにする
    - コンストラクタによる分岐を行うと遅い
    - 可能なら`newtype`を用いる(実行時表現はゼロコストである)
- `INLINE`プラグマを使う
- モジュールでexportする関数の数を減らす
- Coreを読む
    - `let`や辞書アクセス、ネストされたラムダ抽象などは遅い
- Unbox型を用いる
    - Box型からUnbox型への変換は一部GHCも行う
    - Unboxed Arrayを用いる
        - `Data.Array.Unboxed`
        - 一部の(primitiveな？)型のUnboxed ArrayをGHCはサポートしている
- `Float`ではなく`Double`を用いる
- ヒープを増やす
    - GCの回数が減る
- コンパクトデータを用いる
    - [ghc-compact](https://hackage.haskell.org/package/ghc-compact)
    - 長期間生存するデータ構造に対してGCの効率を上げる
    - GHCはコンパクトデータに対してはデータ構造全体を1つのオブジェクトとみなし、すべてをGCするか全くGCしないかのどちらかという戦略を取る

## 生成されるプログラムを小さくする

- インライン化を抑制する
    - `-funfolding-use-threshold`オプションによりインライン化を抑制する
- `Read`を用いない
- `strip`を実行ファイルに対して行う

## ヒープの使用量を抑える

- スペースリークがある？
    - プロファイリングによりスペースリークを探す

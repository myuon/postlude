# コンパイラプラグマ

一部のコンパイラ実装は **コンパイラプラグマ** と呼ばれる仕組みを備えている。これはコンパイラに対して命令を追加したりヒントを与えるようなものであるが、これは正確にはHaskell言語の規格には含まれず、よってプログラムの意味を変えない。この章では実際に使われてきた実践をまとめたものである。実装が各プラグマを提供することは必須ではないが、実装によって認識されないプラグマは無視されるべきである。実際に多くの言語拡張が使われていることもあり、実装は以下に述べるLANGUAGEプラグマをサポートすることが強く推奨されている。

トークンとしては、プラグマは `{-##-}` で囲まれる文法であることを除いてはコメントと同じ方法で記される。

### インライン化

```haskell
_decl -> {-# INLINE _qvars #-}
_decl -> {-# NOINLINE _qvars #-}
```

`INLINE`プラグマは、コンパイラが指定の変数を使用する際にインライン化するよう指示する。コンパイラは単純な式のインライン化をしばしば自動的に行う。`NOINLINE`プラグマによってインライン化を止めることもある。

### 特殊化

```haskell
_decl -> {-# SPECIALIZE _spec_1, ..., _spec_k #-}   (k >= 1)
_spec -> _vars :: _type
```

特殊化はオーバーロードされた関数が非効率に実行されることを防ぐ目的で使われる。例えば、次のプログラム

```haskell
factorial :: Num a => a -> a  
factorial 0 = 0  
factorial n = n ⋆ factorial (n-1)  
{-# SPECIALIZE factorial :: Int -> Int,  
               factorial :: Integer -> Integer #-}
```

では、`factorial`の呼び出しで、コンパイラが渡されたパラメーターが`Int`または`Integer`であることを検出すると、数値オーバーロードされた方ではなく特殊化された`factorial`を用いる。

### 言語拡張

`LANGUAGE`プラグマはファイルの先頭に記述するプラグマ(ファイルヘッダープラグマ)である。ファイルヘッダープラグマはソースファイルでモジュールキーワードよりも前に置かなければならない。ファイルヘッダープラグマは好きなだけ書くことができるし、コメントよりも先に書いても後に書いてもよい。個々のLANGUAGEプラグマは`LANGUAGE`キーワードで始まり、カンマで区切られた、言語機能の名前の列を続けて書く。

例えば、スコープ付き型変数とCPPによるプリプロセッシングを有効にしたいのであれば、使っているHaskell実装がそれらをサポートしていれば、

```haskell
{-# LANGUAGE ScopedTypeVariables, CPP #-}
```

と書くことができる。

Haskellの実装がソースファイルで要求された特定の言語機能を認識またはサポートしない(または要求された言語機能を組み合わてサポートできない)場合は、いかなる方法によるコンパイルでも、あるいはいかなる方法でそのファイルとHaskell実装を用いても、エラーによって失敗しなければならない。

移植性の観点から、サポートされた同じ言語機能を有効にする場合はどのような仕方でも明確に認められている(例: コマンドライン引数、あるいは実装が指定する依存関係や標準的でないプラグマを経由するなど)。`LANGUAGE`プラグマをサポートするHaskell 2010実装は

```haskell
{-# LANGUAGE Haskell2010 #-}
```

をサポートしなければならない。

そのような実装はさらに次のような名前の言語機能をサポートすることが推奨されている:

```haskell
PatternGuards, NoNPlusKPatterns, RelaxedPolyRec,  
EmptyDataDecls, ForeignFunctionInterface
```

これらはHaskell 2010以前、一部の実装でサポートされていたものがこのレポートに入ることになった言語拡張である。


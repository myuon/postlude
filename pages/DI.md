# DI (Dependency Injection)

## Minimal Cake Pattern

ScalaなどのDI手法としてMinimal Cake Patternと呼ばれるパターンが知られている。これは存在型とAd-hoc Polymorphismを用いてDIを実現する手法である。

以下は疑似コードである。

#### 宣言

初めに、関数が定義された`Logger`というインターフェイスがある。これの実装を外から注入するという操作を行う。

```haskell
interface Logger = {
    writeLog : Self -> String -> ()
}
```

`Logger` interfaceを実装した型全体に渡って存在型でunionをとり、`SomeLogger`型と名前を付ける。この存在型は`Logger` interfaceを実装した型たちの性質を受け継ぐので`Logger`の実装を与えることができる。

```haskell
type SomeLogger = exist {t | t implements Logger}

interface SomeLogger implements Logger {
    writeLog = \(self :: SomeLogger) text ->
        case self of
            (self' :: t {- where t implements Logger -}) -> writeLog self' text
    ...
}
```

#### 利用

`SomeLogger`型は実際の型に言及することなく`Logger`型のメソッドを利用することができる。これにより、`SomeLogger`型を受け取った関数は実際に渡される型に(見た目の上では)依存せずにプログラムを記述することができる。

#### 注入

これをDIとして用いるためには、`Logger`の実装を与える`LoggerImpl`を`SomeLogger`型に変換したのちにそれを適切な関数の引数として渡すことでDI(依存性の注入)という操作になる。
Scalaでは型の定義は(オブジェクト指向における)クラス宣言による行うので、注入はコンストラクタの生成時に一緒に指定することになる。

## Haskell

Haskellでも`-XExistentialQuantification`とtypeclassにより同様の記述が可能である。念のため書き下してみる。

```haskell
class Logger a where
    writeLog :: a -> String -> IO ()

data SomeLogger = forall a. Logger a => SomeLogger a

instance Logger SomeLogger where
    writeLog (SomeLogger i) = writeLog i
```

#### 一意性とreflection

さて、`Logger`の実装である`LoggerImpl`を注入するにはどのようにすればいいのだろうか。`LoggerImpl`型を`SomeLogger`型へ変換するだけならば`SomeLogger`コンストラクタを用いればよい。
ところで、このようなDIを行う際、プログラムの実行時に2つ以上の異なる実装が同じインターフェイスに対して与えられることはほとんどない。上の例であれば、通常ロガーとして2つ以上のものを指定することはない。よって、同じインターフェイスに注入される型は(プログラムの実行に対して)一意であると仮定してもよいであろう。

型に対する一意な値の指定(注入)を行うには、[reflection](https://hackage.haskell.org/package/reflection-2.1.4)が便利である。

```haskell
type UseLogger = Given SomeLogger

useLogger :: UseLogger => SomeLogger
useLogger = given
```

ここでは`SomeLogger`への値の注入は`UseLogger`なる型制約を用いて書くことにした。これにより、ロガーを用いて何かを行う処理は次のように書くことができる。

```haskell
doSomething :: UseLogger => IO ()
doSomething = do
    writeLog useLogger "[DEBUG] start doSomething function"
    ...
    writeLog useLogger "[DEBUG] finish doSomething function"
```

また、reflectionのGiven型クラスはプログラムの最も外側で、`give :: a -> (Given a => r) -> r`を用いて与えることができる。例えば`LoggerStdoutImpl`なるロガーの実装があり、`newLoggerStdoutImpl :: LoggerStdoutImpl`を注入する場合には、`give newLoggerStdoutImpl :: (UseLogger => r) -> r`を用いればよいことが分かるだろう。

---

存在型とAd-hoc polymorphismによりDIを実現するのがMinimal Cake Patternである。さらに注入したい実装がプログラム中で一意であれば、reflectionを用いるのが便利であることをみた。

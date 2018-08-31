# Control.Concurrent.MVar

See: [http://hackage.haskell.org/package/base-4.11.1.0/docs/Control-Concurrent-MVar.html](http://hackage.haskell.org/package/base-4.11.1.0/docs/Control-Concurrent-MVar.html)

### MVar

`MVar a`は`a`型のmutable locationを表し、`empty` or `full`の2状態のいずれかを取る。

#### basic operations

basic operationは`putMVar`と`takeMVar`の2つ。`putMVar`はMVarがemptyであれば値を書き込み、状態をfullにする。そうでなければblockする。

|operation|empty|full|
|--|--|--|
|`putMVar`|write & full|block|
|`takeMVar`|block|read & empty|

non-blockingなfunctionが欲しい場合は`tryTakeMVar :: MVar a -> IO (Maybe a)`や`tryPutMVar`などを用いよ。



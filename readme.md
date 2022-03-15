# VBAっぽいコードのインタプリタ

```bash
# antlr4rust (https://github.com/rrevenantt/antlr4rust) を使ってg4からrustコードを生成し、src/gen以下に置く
wget https://github.com/rrevenantt/antlr4rust/releases/download/antlr4-4.8-2-Rust-0.2/antlr4-4.8-2-SNAPSHOT-complete.jar
java -jar ./antlr4-4.8-2-SNAPSHOT-complete.jar -Dlanguage=Rust grammars/vba.g4 -visitor
cp grammars/*.{interp,tokens,rs} src/gen/

# test/test1.basを実行
cargo +nightly-2022-02-23 run test/test1.bas
```

TODO: 機能拡張

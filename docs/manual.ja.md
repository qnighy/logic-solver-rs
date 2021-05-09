# logic-solver マニュアル

logic-solverは様々な論理式の正しさを判定し、証明図を生成するツールです。

## インストール方法

cargo install を使って最新版をインストール:

```
cargo install --git https://github.com/qnighy/logic-solver-rs
```

または、最新版のソースを取得してビルド

```
git clone https://github.com/qnighy/logic-solver-rs.git
cd logic-solver-rs
cargo run
```

## コマンドラインからの使用

```
# インストールしたものを実行
logic-solver -e 'A → A'
# ソースから実行
cargo run -- -e 'A → A'
```

証明可能なときはProvable, そうでないときはNot Provableが出力されます。

(コマンドラインでの証明図出力は未対応)

## LaTeX出力

```
logic-solver -e 'A → A' --latex > result.tex
```

TikZのグラフレイアウト機能に依存するため、コンパイルにはLuaLaTeXが必要です。

```
lualatex result.tex
```

## Twitterからの利用

未実装。

本プログラムは [@ipc_bot](https://twitter.com/ipc_bot) で使われている [旧エンジン](https://github.com/qnighy/ipc_solver) を置き換える目的で作っています。 当面の間は現行の[@ipc_bot](https://twitter.com/ipc_bot)を使ってください。


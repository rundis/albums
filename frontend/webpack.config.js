var CleanWebpackPlugin = require('clean-webpack-plugin');
var CopyWebpackPlugin = require('copy-webpack-plugin');

module.exports = {
  entry: './src/index.js',

  output: {
    path: './dist',
    filename: 'index.js'
  },

  resolve: {
    modulesDirectories: ['node_modules'],
    extensions: ['', '.js', '.elm']
  },

  module: {
    loaders: [
      {
        test: /\.html$/,
        exclude: /node_modules/,
        loader: 'file?name=[name].[ext]'
      },
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        loader: 'elm-hot!elm-webpack'
      }
    ],

    noParse: /\.elm$/
  },

  plugins: [
    new CleanWebpackPlugin(['dist'], {
      root: __dirname,
      verbose: true,
      dry: false
    }),
    new CopyWebpackPlugin([
      { from: 'src/assets', to: 'assets'}
    ])
  ],

  devServer: {
    historyApiFallback: true,
    stats: 'errors-only'
  }
};

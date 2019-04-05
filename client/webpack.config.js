const path = require('path')
const webpack = require('webpack')

module.exports = {
  entry: {
    comic: './src/index.tsx',
    dev: './src/dev.tsx',
  },
  output: {
    path: path.resolve(__dirname, 'dist'),
    filename: '[name].js',
  },
  module: {
    rules: [
      {
        test: /\.tsx?$/,
        use: 'ts-loader',
        exclude: /node_modules/,
      },
    ],
  },
  resolve: {
    extensions: ['.tsx', '.ts', '.js'],
  },
  plugins: [
    new webpack.BannerPlugin('kurita client by chromako.de.'),
  ],
  devServer: {
    contentBase: path.resolve(__dirname, 'dist'),
  },
}

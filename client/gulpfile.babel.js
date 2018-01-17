'use strict';

import gulp from 'gulp';
import elm from 'gulp-elm';
import plumber from 'gulp-plumber';
import sass from 'gulp-sass';
import uglify from 'gulp-uglify';
import minify from 'gulp-minify-css';
import inline from 'gulp-inline-source';
import replace from 'gulp-replace';
import git from 'git-rev-sync';
import identity from 'gulp-identity';
import yargs from 'yargs';


const minifyJs = yargs.argv.production ? uglify : identity;
const minifyCss = yargs.argv.production ? minify : identity;


const dir = {
  dev: './static/dev',
  build: './static/build'
};


// ELM
gulp.task('init', elm.init);

gulp.task('multi', ['init'], () => {
  return gulp.src('elm/Main.elm')
    .pipe(plumber())
    .pipe(elm.make({filetype: 'js', warn: true}))
    .pipe(minifyJs())
    .pipe(gulp.dest(dir.build));
});


// SASS
gulp.task('sass', () => {
  return gulp.src('./sass/**/*.scss')
    .pipe(plumber())
    .pipe(sass().on('error', sass.logError))
    .pipe(minifyCss())
    .pipe(gulp.dest(dir.build));
});


// HTML
gulp.task('html', () => {
  return gulp.src('./html/**/*.html')
    .pipe(inline())
    .pipe(replace('{{git}}', git.long().substring(0, 7)))
    .pipe(gulp.dest(dir.build));
});


// COPY
gulp.task('copy', () => {
  return gulp.src(`${dir.dev}/**`)
    .pipe(gulp.dest(dir.build));
});


// COPYDEPS
gulp.task('copyDeps', () => {
  return gulp.src("./node_modules/howler/dist/howler.min.js")
    .pipe(gulp.dest(dir.build));
});


// DEFAULT
gulp.task('default', ['build', 'watch']);

gulp.task('build', ['multi', 'sass', 'html', 'copy', 'copyDeps']);

gulp.task('watch', () => {
  gulp.watch('elm/**/*.elm', ['multi']);
  gulp.watch('./sass/**/*.scss', ['sass']);
  gulp.watch(`${dir.dev}/**`, ['copy']);
  gulp.watch("node_modules/**", ['copyDeps']);
  gulp.watch(`${dir.dev}/html/*`, ['html']);
});

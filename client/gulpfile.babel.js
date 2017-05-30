'use strict';

import gulp from 'gulp';
import elm from 'gulp-elm';
import plumber from 'gulp-plumber';
import sass from 'gulp-sass';
import uglify from 'gulp-uglify';
import minify from 'gulp-minify-css';
import inline from 'gulp-inline-source';


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
    .pipe(uglify())
    .pipe(gulp.dest(dir.build));
});


// SASS
gulp.task('sass', () => {
  return gulp.src('./sass/**/*.scss')
    .pipe(plumber())
    .pipe(sass().on('error', sass.logError))
    .pipe(minify())
    .pipe(gulp.dest(dir.build));
});


// HTML
gulp.task('html', () => {
  return gulp.src('./html/**/*.html')
    .pipe(inline())
    .pipe(gulp.dest(dir.build));
});


// COPY
gulp.task('copy', () => {
  return gulp.src(`${dir.dev}/**`)
    .pipe(gulp.dest(dir.build));
});


// DEFAULT
gulp.task('default', ['multi', 'sass', 'html', 'copy']);

gulp.task('watch', () => {
  gulp.watch('elm/**/*.elm', ['multi']);
  gulp.watch('./sass/**/*.scss', ['sass']);
  gulp.watch(`${dir.dev}/*`, ['copy']);
  gulp.watch(`${dir.dev}/html/*`, ['html']);
});

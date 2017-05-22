'use strict';

var gulp = require('gulp');
var elm  = require('gulp-elm');
var plumber = require('gulp-plumber');
var sass = require('gulp-sass');
var uglify = require('gulp-uglify');
var minify = require('gulp-minify-css');


// ELM

gulp.task('init', elm.init);

gulp.task('multi', ['init'], function(){
  return gulp.src('elm/Main.elm')
    .pipe(plumber())
    .pipe(elm.make({filetype: 'js', warn: true}))
    .pipe(uglify())
    .pipe(gulp.dest('./static/build/'));
});


// SASS

gulp.task('sass', function () {
  return gulp.src('./sass/**/*.scss')
    .pipe(plumber())
    .pipe(sass().on('error', sass.logError))
    .pipe(minify())
    .pipe(gulp.dest('./static/build/'));
});


// COPY

gulp.task('copy', function () {
  return gulp.src('./static/dev/**')
    .pipe(gulp.dest('./static/build/'));
});


// DEFAULT

gulp.task('default', ['multi', 'sass', 'copy']);

gulp.task('watch', function(){
  gulp.watch('elm/**/*.elm', ['multi']);
  gulp.watch('./sass/**/*.scss', ['sass']);
  gulp.watch('./static/dev/**', ['copy']);
});

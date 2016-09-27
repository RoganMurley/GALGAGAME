var gulp = require('gulp');
var elm  = require('gulp-elm');
var plumber = require('gulp-plumber');

gulp.task('init', elm.init);

gulp.task('multi', ['init'], function(){
  return gulp.src('elm/*.elm')
    .pipe(plumber())
    .pipe(elm.make({filetype: 'js'}))
    .pipe(gulp.dest('static/'));
});

gulp.task('watch', function(){
  return gulp.watch('elm/*.elm', ['multi']);
});

gulp.task('default', ['multi', 'watch']);

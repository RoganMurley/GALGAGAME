var gulp = require('gulp');
var elm  = require('gulp-elm');

gulp.task('init', elm.init);

gulp.task('multi', ['init'], function(){
  return gulp.src('elm/*.elm')
    .pipe(elm.make({filetype: 'js'}))
    .pipe(gulp.dest('static/'));
});

gulp.task('watch', function(){
  return gulp.watch('elm/*.elm', ['multi']);
});

gulp.task('default', ['watch']);

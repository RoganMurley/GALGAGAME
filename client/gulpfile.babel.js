import gulp from 'gulp';
import elm from 'gulp-elm';
import sass from 'gulp-sass';
import uglify from 'gulp-uglify';
import minify from 'gulp-minify-css';
import inline from 'gulp-inline-source';
import replace from 'gulp-replace';
import git from 'git-rev-sync';
import identity from 'gulp-identity';
import yargs from 'yargs';


const { production } = yargs.argv;
const minifyJs = production ? uglify : identity;
const minifyCss = production ? minify : identity;


const dir = {
  dev: 'static/dev',
  build: 'static/build'
};


// ELM
gulp.task('elm', () => {
  return gulp.src('elm/Main.elm')
    .pipe(elm.make({filetype: 'js', optimize: true}))
    .pipe(minifyJs())
    .pipe(gulp.dest(dir.build));
});


// SASS
gulp.task('sass', () => {
  return gulp.src('sass/**/*.scss')
    .pipe(sass().on('error', sass.logError))
    .pipe(minifyCss())
    .pipe(gulp.dest(dir.build));
});


// HTML
gulp.task('html', () => {
  return gulp.src('html/**/*.html')
    .pipe(inline({ compress: production }))
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
  return gulp.src('node_modules/howler/dist/howler.min.js')
    .pipe(gulp.dest(dir.build));
});


// DEFAULT
gulp.task('build', gulp.parallel('elm', 'sass', 'html', 'copy', 'copyDeps'));

gulp.task('watch', () => {
  gulp.watch('elm/**/*.elm', gulp.series('elm'));
  gulp.watch('sass/**/*.scss', gulp.series('sass'));
  gulp.watch(`${dir.dev}/**`, gulp.series('copy'));
  gulp.watch("node_modules/**", gulp.series('copyDeps'));
  gulp.watch(`html/index.html`, gulp.series('html'));
  gulp.watch(`js/start.js`, gulp.series('html'));
});

gulp.task('default', gulp.series('build', 'watch'));

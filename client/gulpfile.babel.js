import fs from 'fs';
import gulp from 'gulp';
import elm from 'gulp-elm';
import file from 'gulp-file';
import filter from 'gulp-filter';
import handlebars from 'gulp-compile-handlebars';
import sass from 'gulp-sass';
import uglify from 'gulp-uglify';
import minify from 'gulp-minify-css';
import inline from 'gulp-inline-source';
import identity from 'gulp-identity';
import rename from 'gulp-rename';
import rev from 'gulp-rev';
import yargs from 'yargs';

const dir = {
  dev: 'static/dev',
  build: 'static/build'
};

const { debug, production } = yargs.argv;
const minifyJs = production ? uglify : identity;
const minifyCss = production ? minify : identity;
const optimize = !debug;

let revJs;
if (production) {
  revJs = x => (
    x.pipe(rev())
      .pipe(gulp.dest(dir.build))
      .pipe(rev.manifest('js-manifest.json'))
  );
} else {
  revJs = x => (
    x.pipe(file('js-manifest.json', '{"Main.js": "Main.js"}'))
      .pipe(gulp.dest(dir.build))
  );
}

// ELM
gulp.task('elm', () => {
  return revJs(
    gulp.src('elm/Main.elm')
      .pipe(elm.make({ filetype: 'js', optimize }))
      .pipe(minifyJs())
  ).pipe(gulp.dest(dir.build));
});


// SASS
gulp.task('sass', () => {
  return gulp.src('sass/**/*.scss')
    .pipe(sass().on('error', sass.logError))
    .pipe(minifyCss())
    .pipe(gulp.dest(dir.build));
});


// HTML
var handlebarOpts = {
  batch: ['./html/partials'],
  helpers: {
    assetPath: function (path, context) {
      return ['/', context.data.root[path]].join('');
    }
  }
};

gulp.task('html', () => {
  const manifest = JSON.parse(fs.readFileSync(`${dir.build}/js-manifest.json`, 'utf8'));
  return gulp.src('html/*.hbs')
    .pipe(inline({ compress: production }))
    .pipe(handlebars(manifest, handlebarOpts))
    .pipe(rename(path => {
      if (path.extname === '.html') {
        return;
      }
      return {
        dirname: path.dirname,
        basename: path.basename,
        extname: '.html'
      };
    }))
    .pipe(gulp.dest(dir.build));
});


// COPY
gulp.task('copy', () => {
  return gulp.src(`${dir.dev}/**`)
    .pipe(gulp.dest(dir.build))
    .pipe(filter([`**`, `!${dir.build}/favicons/*`, `!${dir.build}/img/landing/*.png`]))
    .pipe(rev())
    .pipe(gulp.dest(dir.build))
    .pipe(rev.manifest())
    .pipe(gulp.dest(dir.build));
});


// COPYDEPS
gulp.task('copyDeps', () => {
  return gulp.src('node_modules/howler/dist/howler.min.js')
    .pipe(gulp.dest(dir.build));
});


// DEFAULT
gulp.task('build', gulp.parallel(gulp.series('elm', 'html'), 'sass', 'copy', 'copyDeps'));

gulp.task('watch', () => {
  gulp.watch('elm/**/*.elm', gulp.series('elm', 'html'));
  gulp.watch('sass/**/*.scss', gulp.series('sass'));
  gulp.watch(`${dir.dev}/**`, gulp.series('copy'));
  gulp.watch("node_modules/**", gulp.series('copyDeps'));
  gulp.watch(`html/*.hbs`, gulp.series('html'));
  gulp.watch(`js/start.js`, gulp.series('html'));
});

gulp.task('default', gulp.series('build', 'watch'));

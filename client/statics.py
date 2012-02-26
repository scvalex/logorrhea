#!/usr/bin/env python

ROOT_DIR = '.'

STATICS = {
  'js/require.js': 'http://requirejs.org/docs/release/1.0.7/minified/require.js',
  'js/coffee-script.js': 'http://github.com/jashkenas/coffee-script/raw/master/extras/coffee-script.js',
  'js/jquery.js': 'http://code.jquery.com/jquery.min.js',
  'js/mootools.js': 'http://mootools.net/download/get/mootools-core-1.4.4-full-nocompat.js',
  'js/knockout.js': 'https://github.com/downloads/SteveSanderson/knockout/knockout-2.0.0.js',
  'js/jasmine.js': 'https://raw.github.com/pivotal/jasmine/master/lib/jasmine-core/jasmine.js',
  'js/jasmine-html.js': 'https://raw.github.com/pivotal/jasmine/master/lib/jasmine-core/jasmine-html.js',
  'css/jasmine.css': 'https://raw.github.com/pivotal/jasmine/master/lib/jasmine-core/jasmine.css',
  'css/bootstrap.css': 'http://twitter.github.com/bootstrap/assets/css/bootstrap.css',
  'css/bootstrap-responsive.css': 'http://twitter.github.com/bootstrap/assets/css/bootstrap-responsive.css',
}

if __name__ == '__main__':
    import staticfetcher
    staticfetcher.Staticfetcher(STATICS, ROOT_DIR).run()

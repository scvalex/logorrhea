#!/usr/bin/env python


ROOT_DIR = '.'

STATICS = {
	'js/coffee-script.js': 'http://github.com/jashkenas/coffee-script/raw/master/extras/coffee-script.js',
	'js/jquery.js': 'http://code.jquery.com/jquery.min.js',
	'js/mootools.js': 'http://mootools.net/download/get/mootools-core-1.4.4-full-nocompat.js',
	'js/knockout.js': 'https://github.com/downloads/SteveSanderson/knockout/knockout-2.0.0.js',
	'js/sockjs.js': 'http://cdn.sockjs.org/sockjs-0.2.min.js',
        'js/jasmine.js': 'https://raw.github.com/pivotal/jasmine/master/lib/jasmine-core/jasmine.js',
        'js/jasmine-html.js': 'https://raw.github.com/pivotal/jasmine/master/lib/jasmine-core/jasmine-html.js',
        'css/jasmine.css': 'https://raw.github.com/pivotal/jasmine/blob/master/lib/jasmine-core/jasmine.css',
}


if __name__ == '__main__':
	import staticfetcher
	staticfetcher.Staticfetcher(STATICS, ROOT_DIR).run()

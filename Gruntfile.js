module.exports = function(grunt) {
    // Project configuration
    grunt.initConfig({
        pkg: {
            "devDependencies": {
                "grunt-sass": "~1.1.0-beta",
                "node-sass": "~3.3.3"
            }
        },

        sass: {
            themes: {
                files: [
                    {
                        expand: true,
                        cwd: 'sass',
                        src: ['*.scss'],
                        dest: 'reveal.js/css/theme',
                        ext: '.css'
                    }
                ]
            }
        }
    });

    // Dependencies
    grunt.loadNpmTasks( 'grunt-sass' );

    // Default task
    grunt.registerTask( 'default', [ 'sass:themes' ] );

    // Theme CSS
    grunt.registerTask( 'css-themes', [ 'sass:themes' ] );
};

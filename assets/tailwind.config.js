module.exports = {
    theme: {
        fontFamily: {
            'sans': ["Recursive Sans"],
            'serif': ["Recursive Sans Casual"],
            'mono': ["Recursive Mono"],
        },
    },
    purge: [
        '../lib/**/*.ex',
        '../lib/**/*.leex',
        '../lib/**/*.eex',
        './js/**/*.js'
    ],
    variants: {},
    plugins: [
        function({ addUtilities }) {
            const newUtilities = {
                '.stroke-width-1': {
                    'stroke-width': 1,
                },
                '.stroke-width-2': {
                    'stroke-width': 2,
                },
                '.stroke-width-4': {
                    'stroke-width': 4,
                },
                '.r-2': {
                    r: 2,
                },
                '.r-4': {
                    r: 4,
                },
            };

            addUtilities(newUtilities, ['responsive', 'hover']);
        }
    ],
}


const colors = require('tailwindcss/colors')

module.exports = {
    theme: {
        fontFamily: {
            'sans': ["Recursive Sans"],
            'serif': ["Recursive Sans Casual"],
            'mono': ["Recursive Mono"],
        },
        colors: {
            // Build your palette here
            transparent: 'transparent',
            current: 'currentColor',
            gray: colors.trueGray,
            black: colors.black,
            white: colors.white,
            red: colors.red,
            blue: colors.lightBlue,
            yellow: colors.amber,
            teal: colors.teal,
        }
    },
    purge: [
        '../lib/**/*.ex',
        '../lib/**/*.leex',
        '../lib/**/*.eex',
        './js/**/*.js'
    ],
    variants: {},
    plugins: [
        require('@tailwindcss/forms'),
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


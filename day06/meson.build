project('signal', 'vala', 'c')

deps = [
    dependency('glib-2.0'),
    dependency('gobject-2.0'),
    dependency('gee-0.8'),
]

sources = files(
    'main.vala',
)

executable('signal',
    sources,
    dependencies: deps,
    install: false,
)

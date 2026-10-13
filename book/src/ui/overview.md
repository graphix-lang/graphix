# Building UIs With Graphix

Graphix excels at building user interfaces thanks to its reactive dataflow nature. Changes in data automatically propagate through the UI graph, updating only the components that need to change. This makes building complex, interactive applications surprisingly straightforward.

## Why Graphix for UIs?

Traditional UI frameworks require you to manually manage state changes, update DOM elements, and coordinate between different parts of your application. Graphix eliminates this complexity by treating your entire application as a reactive graph where:

- **Data flows automatically**: When underlying data changes, dependent UI components update automatically
- **State is declarative**: You describe what the UI should look like, not how to update it
- **Composition is natural**: Complex UIs are built by composing simple, reusable components
<!-- CR claude for claude: [doc-drift/low] ui/overview.md: 'only components that depend
on changed data re-render' is not what either backend does. The dataflow graph
recomputes only changed nodes, but rendering is whole-tree in both backends. The TUI
redraws the entire widget tree on every dirty cycle (graphix-package-tui/src/lib.rs,
`terminal.draw` over the root; ratatui then diffs cells). The GUI rebuilds iced's view
for the whole window. A reader who sizes a large UI on this promise will be surprised.
Failure: A user builds a TUI whose one widget is a 10k-row table and a clock, expecting
the clock tick to touch only the clock. Every tick redraws the table. Fix: Say what is
true: 'only the computations that depend on changed data are re-run; the view is then
redrawn from the current widget values'. Unverified: the reviewer's report, not checked
by a verifier. (book-ui-b-11) -->
- **Performance is built-in**: Only components that depend on changed data will re-render

## UI Backends

Graphix currently supports two UI backends:

### Terminal UIs (TUIs)

Surprisingly complex and useful UIs can be built in the standard terminal, and it is the absolute lowest common denominator that will always be present even on a bandwidth constrained remote system. Graphix uses the excellent ratatui library as a basis to build upon.

### Graphical UIs (GUIs)

Native desktop applications with GPU-accelerated rendering, built on the iced framework. GUI programs return `Array<&Window>` (aliased as `gui::Gui`) to create one or more windows with rich widget trees, theming, and the same reactive programming model as TUIs.

## Future UI Targets

The reactive architecture makes Graphix well-suited for additional UI paradigms:

- **Web UIs**: The dataflow model maps naturally to modern web frameworks
- **Mobile UIs**: Touch-based interfaces with gesture handling

The core concepts of reactive data flow, component composition, and declarative styling apply across all UI targets.

## Getting Started

The Graphix shell automatically detects the UI backend from the type of your program's last value:

- `tui::Tui` — launches a terminal UI
- `gui::Gui` (i.e. `Array<&Window>`) — launches a graphical desktop UI

You can try out the examples in this book by pasting them in a file, or even typing (the short ones) into the interactive REPL. Each component has detailed documentation in the following sections, including complete API references and practical examples.

TUI examples are in `examples/tui/`, GUI examples in `examples/gui/`.

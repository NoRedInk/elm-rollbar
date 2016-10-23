# elm-rollbar
Rollbar helpers for Elm


Example

```elm
import Rollbar

rollbarReporter = Rollbar.scopedRollbar "ExamplePage"

update model =
    SomeError error =
        (model, rollbarReporter.warning error)
    SomeCritical error =
        (model, rollbarReporter.critial error)

```

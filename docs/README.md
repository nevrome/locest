Easy setup of sphinx with an apptainer container:
```
apptainer build docs/sphinx.sif docs/sphinx.def
```

Build documentation (in proejct root to have .git ready):
```
apptainer exec docs/sphinx.sif sphinx-multiversion docs docs/_build/html
```

Version distinction is done with git tags.

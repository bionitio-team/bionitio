# Overview 

Generate bionitio README.md files based on a template.

This allows us to make sure all the README files for bionitio are consistent, and only differ where necessary due to
language specific differences.

# Example usage

```
bionitio-readme.sh -t TEMPLATE.md -l python -i $HOME/bionitio-python/readme_includes > $HOME/bionitio-python/README.md
```

# How to update all the bionitio READMEs for all the different language implementations

1. Clone all the repositories using the helpful git wrapper:
```
cd $SCRATCH_DIR
$BIONITIO_SRC/githelper/bionitio-git.sh -c clone
```
2. Run the README template program for each language:
```
cd $BIONITIO_SRC/readme_template/
for lang in 'c clojure cpp csharp haskell java js perl5 python r ruby rust'; do \
    bionitio-readme.sh -t TEMPLATE.md -l $lang -i "$SCRATCH_DIR/bionitio-${lang}/readme_includes" > "$SCRATCH_DIR/bionitio-${lang}/README.md" \
done
```

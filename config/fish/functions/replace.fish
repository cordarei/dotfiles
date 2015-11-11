function replace -d "Replace the given file ($1) with the contents of stdin"
  set tmp (mktemp)
  cp $argv[1] $tmp
  cat >$tmp
  mv $tmp $argv[1]
end

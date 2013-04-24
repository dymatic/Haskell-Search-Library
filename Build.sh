#Commit all changes to git
echo "TESTING FOR SUCCESSFUL COMPILE:"
  ghc --make TESTBUILD.hs
  ./TESTBUILD

read -p "Clean up the object files? [Enter]"
  rm ./TESTBUILD
  find . -name "*.o" -exec rm -f '{}' +
  find . -name "*.hi" -exec rm -f '{}' +

read -p "Commit the changes to git?"
  git add *
  git add -u

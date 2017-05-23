<?php
echo "<form action='Execution.php' method='get'>";
echo "Search Key: <input type='text' name='search_key' />";
echo "<input type='submit' />";
echo "</form>";
  // execute R script from shell
  // this will save a plot at temp.png to the filesystem
if(isset($_GET['search_key']))
{
  $N = $_GET['search_key'];
  exec (".\Project\R\R-3.3.1\bin\Rscript .\Project\get_tweets.R $N");
  exec (".\Project\R\R-3.3.1\bin\Rscript .\Project\Main.R");
  echo "<img src='Project/sentiplot.png'/>";
  exec (".\Project\R\R-3.3.1\bin\Rscript .\Project\Text_Mining.R");
  echo "<img src='Project/freqwordcloud.png'/>";
  echo "<img src='Project/freqwords.png'/>";
  exec (".\Project\R\R-3.3.1\bin\Rscript .\Project\NaiveBayes.R");
  echo "<img src='Project/nb.png'/>";
  exec (".\Project\R\R-3.3.1\bin\Rscript .\Project\SVM.R");
  echo "<img src='Project/maxenttrain.png'/>";
  echo "<img src='Project/svmtrain.png'/>";



}
?>
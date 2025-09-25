chibi-scheme efficiency.sps | 
awk '
{
  if ($1 == "zp") {
    printf("%-3s %.02e\n", $2, $3);
  } else if ($1 == "eta") {
    printf("%-3s %.02f %.02f %.03f\n", $2, $3, $4, $5);
  } else {
    print;
  }
}
'
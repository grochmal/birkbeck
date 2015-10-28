<!DOCTYPE html>
<html>
<head>
<title>a php page</title>
</head>
<body>
<h3>Below will be some php generated text</h3>
<?php
//php_info();
$_POST = "yay!";
$_POST || $_POST = array( "url" => "http://sth", "mime" => "text/html" );
var_dump($_GET);
echo "<p>a PHP paragraph</p>\n";
var_dump($_POST);
?>
</body>
</html>


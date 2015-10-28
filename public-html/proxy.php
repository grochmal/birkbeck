<?php
$argv = $_GET ? $_GET : $_POST ? $_POST :
array( 'action' => 'debug'
     , 'url'    => 'http://fsf.org'
     , 'mime'   => 'text/html'
     );
if ('debug' === $argv['action']) var_dump($argv);
if ('info'  === $argv['action']) php_info();
if ('proxy' === $argv['action']) {
    header("Content-type: $argv[mime]; charset=utf-8");
    echo file_get_contents($argv['url']);
}
?>


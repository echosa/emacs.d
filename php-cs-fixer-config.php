#!/usr/bin/php
<?php

error_reporting(E_ALL | E_STRICT);

$finder = \PhpCsFixer\Finder::create()->in(__DIR__);

return \PhpCsFixer\Config::create()
    ->setRules([
        '@Symfony' => true,
        'array_syntax' => ['syntax' => 'short'],
        'yoda_style' => false,
        'phpdoc_summary' => false,
    ])->setFinder($finder);

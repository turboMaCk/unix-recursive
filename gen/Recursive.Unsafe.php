<?
$module_name = "System.Posix.Recursive.Unsafe";
$file_path_type = "FilePath";
$check_empty_fc = "null";
$safe = false;

$imports = <<<HS
    import qualified System.Posix.Directory as Posix
    import qualified System.Posix.Files as Posix

    HS;

include 'MetaModule.hs';
?>

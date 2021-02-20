<?
$module_name = "System.Posix.Recursive.ByteString";
$file_path_type = "RawFilePath";
$check_empty_fc = "BS.null";
$safe = true;

$imports = <<<HS
    import System.Posix.ByteString.FilePath (RawFilePath)
    import qualified Data.ByteString as BS

    import qualified System.Posix.Directory.ByteString as Posix
    import qualified System.Posix.Files.ByteString as Posix

    HS;

include 'MetaModule.hs';
?>

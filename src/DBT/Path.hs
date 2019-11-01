module DBT.Path
  ( pgData
  , pgHBAAbs
  , pgHome
  , pgMasterPasswordAbs
  )
where

import System.Path

pgData :: AbsDir
pgData = pgHome </> relDir "data"

pgHBAAbs :: AbsFile
pgHBAAbs = pgData </> pgHBARel

pgHBARel :: RelFile
pgHBARel = relFile "pg_hba.conf"

pgHome :: AbsDir
pgHome = absDir "/var/lib/postgresql"

pgMasterPasswordAbs :: AbsFile
pgMasterPasswordAbs = pgHome </> relFile "master-password.txt"

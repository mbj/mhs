module DBT.Path
  ( pgData
  , pgHBAConfAbs
  , pgHome
  , pgMasterPasswordAbs
  )
where

import System.Path

pgData :: AbsDir
pgData = pgHome </> relDir "data"

pgHBAConfAbs :: AbsFile
pgHBAConfAbs = pgData </> file "pg_hba.conf"

pgHome :: AbsDir
pgHome = absDir "/var/lib/postgresql"

pgMasterPasswordAbs :: AbsFile
pgMasterPasswordAbs = pgHome </> relFile "master-password.txt"

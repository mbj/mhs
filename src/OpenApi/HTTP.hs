module OpenApi.HTTP where

import Numeric.Natural (Natural)
import OpenApi.Prelude

import qualified Network.HTTP.Types.Status as HTTP

-- Convert safely from natural to known http status.
--
-- Both `toEnum` and upstream `mkStatus` allow to create odd
-- statuses, like negative or unregistered ones.
mkStatus :: Natural -> Maybe HTTP.Status
mkStatus = \case
  100 -> pure HTTP.status100
  101 -> pure HTTP.status101
  200 -> pure HTTP.status200
  201 -> pure HTTP.status201
  202 -> pure HTTP.status202
  203 -> pure HTTP.status203
  204 -> pure HTTP.status204
  205 -> pure HTTP.status205
  206 -> pure HTTP.status206
  300 -> pure HTTP.status300
  301 -> pure HTTP.status301
  302 -> pure HTTP.status302
  303 -> pure HTTP.status303
  304 -> pure HTTP.status304
  305 -> pure HTTP.status305
  307 -> pure HTTP.status307
  308 -> pure HTTP.status308
  400 -> pure HTTP.status400
  401 -> pure HTTP.status401
  402 -> pure HTTP.status402
  403 -> pure HTTP.status403
  404 -> pure HTTP.status404
  405 -> pure HTTP.status405
  406 -> pure HTTP.status406
  407 -> pure HTTP.status407
  408 -> pure HTTP.status408
  409 -> pure HTTP.status409
  410 -> pure HTTP.status410
  411 -> pure HTTP.status411
  412 -> pure HTTP.status412
  413 -> pure HTTP.status413
  414 -> pure HTTP.status414
  415 -> pure HTTP.status415
  416 -> pure HTTP.status416
  417 -> pure HTTP.status417
  418 -> pure HTTP.status418
  422 -> pure HTTP.status422
  426 -> pure HTTP.status426
  428 -> pure HTTP.status428
  429 -> pure HTTP.status429
  431 -> pure HTTP.status431
  500 -> pure HTTP.status500
  501 -> pure HTTP.status501
  502 -> pure HTTP.status502
  503 -> pure HTTP.status503
  504 -> pure HTTP.status504
  505 -> pure HTTP.status505
  511 -> pure HTTP.status511
  _   -> empty

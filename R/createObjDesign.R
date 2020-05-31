createObjDesignNormal = function(instances, psOpt, info) {
  objNormal = list(
    objNormal = data.table::data.table(

      instances = instances,

      psOpt = list(psOpt),

      info = list(info)
    )
  )
}

createObjDesignEncoded = function(instances, psOpt, info) {
  objEncoded = list(
    objEncoded = data.table::data.table(

      instances = instances, #####list() weg

      psOpt = list(psOpt),

      info = list(info)
    )
  )
}


createObjDesignEncodedSpot = function(instances, psOpt, info) {
  objEncodedSpot = list(
    objEncodedSpot = data.table::data.table(

      instances = instances, #####list() weg

      psOpt = list(psOpt),

      info = list(info)
    )
  )
}

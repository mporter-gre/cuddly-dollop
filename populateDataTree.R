

populateProjects <- function(omConn, userId){
  projList = getProjects(omConn)
  projTbl = tibble(name = character(), id = numeric(), owner = numeric())
  print(userId)
  for (thisProj in projList){
    projName <- thisProj@dataobject$getName()
    projId <- thisProj@dataobject$getId()
    projOwner <- getProjectOwner(thisProj)
    projTbl <- add_row(projTbl, name = projName, id = projId, owner = projOwner)
  }
  projTbl <- filter(projTbl, owner == userId)
  projTbl <- arrange(projTbl, name, id)
  return(projTbl)
}


populateDatasets <- function(omConn, projId){
  try({
    proj <- loadObject(omConn, 'ProjectData', projId)
    dsList <- getDatasets(proj)
    dsTbl <- tibble(name = character(), id = numeric())
    for (thisDs in dsList){
      dsName <- thisDs@dataobject$getName()
      dsId <- thisDs@dataobject$getId()
      dsTbl <- add_row(dsTbl, name = dsName, id = dsId)
    }
    dsTbl <- arrange(dsTbl, name, id)
    return(dsTbl)
  })
}

populateImages <- function(omConn, dsId){
  try({
    dsObj <- loadObject(omConn, 'DatasetData', dsId)
    imgList <- getImages(dsObj)
    imgTbl <- tibble(name = character(), id = numeric())
    for (thisImg in imgList){
      imgName <- thisImg@dataobject$getName()
      imgId <- thisImg@dataobject$getId()
      imgTbl <- add_row(imgTbl, name = imgName, id = imgId)
    }
    imgTbl <- arrange(imgTbl, name, id)
    return(imgTbl)
  })
}


getUserId <- function(omConn){
  userObj <- getUser(omConn)
  userId <- userObj$getId()
}

getProjectOwner <- function(projectObj){
  owner <- projectObj@dataobject$getOwner()
  ownerId <- owner$getId()
  }
  
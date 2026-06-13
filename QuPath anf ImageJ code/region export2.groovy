import static qupath.lib.gui.scripting.QPEx.*

server  = getCurrentServer()
path = server.getPath()
downsample = 5.0

name = GeneralTools.getNameWithoutExtension(server.getMetadata().getName())
pathOutput = buildFilePath(PROJECT_BASE_DIR, "patches", name)
pathOutput_region = buildFilePath(PROJECT_BASE_DIR, "patches", name)
pathOutput_region = buildFilePath(PROJECT_BASE_DIR, "patches", name)
mkdirs(pathOutput)
mkdirs(pathOutput_region)
mkdirs(pathOutput_region)


i = 1
j = 1
for (annotation in getAnnotationObjects()){
    roi = annotation.getROI()
    request = RegionRequest.createInstance(path, downsample, roi)
    if (annotation.toString().contains("chironomidae")){
        writeImageRegion(server, request, pathOutput_region + "/region" + i + "_" + roi.toString() + '.jpeg')
        i = i + 1
        }
     else{
        writeImageRegion(server, request, pathOutput_region + "/region" + j + "_" + roi.toString() + '.jpeg')
        j = j + 1
        }
     
}
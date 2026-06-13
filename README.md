STRUCTURE OF DATA

#==================================================================================
#R code and data
Repository includes R code file and accompanying data

1. These files are counts of photos/ specimens per class as identified by ResNet50 network
Files containing raw measurements of the individual specimens of subfossils, conducted by ResNet50.
1a) Class counts by ResNet 50 for lake Ana, Romania: morphology_ana_df.csv
Content of the dataset: "id" – path to the image from which measurments where taken; "image" – short sample´s name; "label" – name of the class (“taxa”); "Lake" – lake from which surface sample originates;"sample" – sample´s full name; "m_majorAxis" – size in µm.
1b) Class counts by ResNet 50 for lake Hijkermeer, Netherlands: morphology_hij_1003_df.csv
Content of the dataset: "id" – path to the image from which measurments where taken; "image" – short sample´s name; "label" – name of the class (“taxa”); "sample" – sample´s full name; "m_majorAxis" – size in µm.
1c) Class counts by ResNet 50 for lake Laguna de Rio Seco, Spain: morphology_ldrs_df.csv
Content of the dataset: "id" – path to the image from which measurments where taken; "image" – short sample´s name; "label" – name of the class (“taxa”); "sample" – sample´s full name; "m_majorAxis" – size in µm; "Age" –sample´s age. 
1d) Class counts by ResNet 50 for lake Laguna de la Roya, Spain: morphology_ldry_df.csv
Content of the dataset: "id" – path to the image from which measurments where taken; "image" – short sample´s name; "label" – name of the class (“taxa”); "sample" – sample´s full name; "m_majorAxis" – size in µm.
Class counts by ResNet 50 for lake Peleaga, Romania: morphology_peleaga_df.csv
Content of the dataset: "id" – path to the image from which measurments where taken; "image" – short sample´s name; "label" – name of the class (“taxa”); "Lake" – lake from which surface sample originates;"sample" – sample´s full name; "m_majorAxis" – size in µm.
Class counts by ResNet 50 for the set of 52 lakes (surface sediments) from Tatra mountains (Poland and Slovakia): morphology_sk1003_df.csv
Content of the dataset: "id" – path to the image from which measurments where taken; "image" – short sample´s name; "label" – name of the class (“taxa”); "Lake" – lake from which surface sample originates;"sample" – sample´s full name; "m_majorAxis" – size in µm.
2. Data for comparison of the manual and automatic measurements of subfossil Chironomidae of selected morphotypes 
File containing manual and automatic measurements: manual_aut_size_compare.csv
Content of the dataset: "T.WATER"-  palaeotemperature reconstructed for the temporal interval, using WAPLS model, or, in case of Tatra set of lakes, measured directly, while sampling surface sediments ;"size_manual"- manual measurements of the specimens; "Taxa" –morphotype´s name; "size_automatic" – automatic measurments, performed by ResNet50; "lake" –lake´s name.
3. Combined file containing sizes measured automatically (mean value per lake per sample per morphotype), by ResNet50, with temperature and lake data. 
Content of the dataset: 1428 aggregated measurements; 9 columns – “X” – index, containing number of the row; “Lake” –name of the lake from which sample comes; “Temperature” – palaeotemperature reconstructed for the temporal interval, using WAPLS model, or, in case of Tatra set of lakes, measured directly, while sampling surface sediments. ; “Depth”- sampling depth of the interval in the core; “Taxa_lake” –combined identifier, for morphotype and lake, to test for lake-specific difference in size trends of various morphotypes; “Taxa” – name of the morphotype; “size” – subfossil´s size in µm; “count” – number of specimens of given morphotype per sample; “data_type” – “downcore” or “surface” to differentiate between downcore and surface sediments samples. 
File: size_temperature_aut2.csv
4. Compare raw manual and automatic counts of non-biting midges in the studies lakes
4a) Tatra lakes, manual counts: sk_counts.csv 
Content of the dataset: dataset, with 53 columns, 1st (“Taxa”) contains morphotypes´ names, and following 52 are names of the lakes, containing taxa count per lake.
4b) Tatra lakes, automatic counts: sk_automatic_reduced.csv
Content of the dataset: "id" – path to the image from which measurments where taken; "image" – short sample´s name; "label" – name of the class (“taxa”); "Lake" – lake from which surface sample originates;"sample" – sample´s full name; "m_majorAxis" – size in µm; “id1” – numeric id of the lake.
4c) Hijkermeer lake, manual counts:  Hijkermeer_count.csv  
Content of the dataset: dataset, with 71 columns, 1st (“depth”), contains depth of the given sample, other 70 are names of the subfossils´ morphotypes, and their counts per depth. 

4d) Hijkermeer lake, automatic counts: hij_aut.csv
Content of the dataset: “Taxa" - name of the morphotypes, "Depth below sediment water interface (cm)" – sample depth. Each row represents one specimen of measured subfossil here.
4e) Laguna de la Roya, manual counts: ROY_counts.csv
Content of the dataset: dataset, with 101 columns, 1st – “sample” –gives a code of the depth slice of the column; 2nd (“Absolute.depth..2010.scale..cm”), contains depth of the given sample, other 99 are names of the subfossils´ morphotypes, and their counts per depth. 

4f) Laguna de la Roya, automatic counts: ldry_aut_1.csv
Content of the dataset: “taxa" - name of the morphotypes, " Absolute.depth" – sample depth. Each row represents one specimen of measured subfossil here.
4g) Laguna de la Roya, automatic counts: LdRS for Viktor July 25.csv
Content of the dataset: dataset, with 24 columns, 1st (“core.LdRS.Core.01” ) gives a code of the core; 2nd (“REAL.DEPTH”), contains depth of the given sample, 3d  (Age..cal.yr.BP.) –gives calibrated age of the sample; other 20 are names of the subfossils´ morphotypes, and their counts per depth, final column (“Total”) is a sum of subfossil per sample. 
4g) Laguna de la Roya, automatic count: ldrs_aut_1.csv
Content of the dataset: “label" - name of the morphotypes, " REAL.DEPTH" – sample depth. Each row represents one specimen of measured subfossil here.
4i) Lake Ana, manual counts: lake_ana_counts.csv
Content of the dataset: dataset contains 16 columns, first containing names of the rows (taxa) and 15 others, age of the sample. Dataset is transposed by code for analysis
4j) Lake Ana, automatic counts: ana_aut_1.csv
Content of the dataset: “Taxa" - name of the morphotypes, " Depth" – sample depth. Each row represents one specimen of measured subfossil here.

4k) Lake Peleaga, manual counts: lake_peleaga_counts.csv 
Content of the dataset: dataset contains 21 columns, first containing names of the rows (taxa) and 20 others, age of the sample. Dataset is transposed by code for analysis
4l) Lake Peleaga, automatic counts: peleaga_aut_1.csv
Content of the dataset: “Taxa" - name of the morphotypes, " Depth" – sample depth. Each row represents one specimen of measured subfossil here.
#=====================================================================================================================================================
Folder "QuPath anf ImageJ code" contains two files
"region export2.groovy" - exports annotated regions of interest (ROI) in QuPath (in case of this code, we were calling ROI "patches"), via "automate"  menu of the QuPath
"AutoViktor_Segmenter.ijm" - segments patches extracted from QuPath into the individual images, suitable for ParticleTrieur and further CNN training. 
Is s Fiji/ImageJ plugin, that should be placed into the "plugins" folder of the Fiji/ImageJ, an executed via "Plugins" menu. In order for code to work, a target directory, in which images, you wish to segment are situated, must be placed into another folder, and this folder, containing target folder, should be selected as an input directory for the plugin.
Modified from Tetard et al. (2020) https://cp.copernicus.org/articles/16/2415/2020/cp-16-2415-2020-discussion.html


#=====================================================================================================================================================
#ResNet50 ONNX model
All this files housed at Zenodo https://zenodo.org/records/18833507

Folder ResNet50_20250806-135512 includes following files and folders 
Folder: model_onnx - contains onnx model, that can be loaded into ParticleTrieur and used for automatic identintification of the images 
Foder: model_tf2 - contains tf2 model, that can be loaded into ParticleTrieur and used for automatic identintification of the images
 you
also, folder contains diagnostoc and output files for the ResNet50 models
/legend.csv
/loss_vs_epoch.pdf
/training_parameters.json
/tsne.pdf
/model_onnx
/model_tf2
/accuracy_vs_epoch.pdf
/confusion_matrix.pdf
/health_summary.txt

folder "Test" contains final training dataset (images)

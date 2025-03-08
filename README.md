<img src="https://github.com/khaors/KohonenF08/blob/main/media/logo.png" width=150% height=150%>

============

# KohonenF08

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
![Fortran](https://img.shields.io/badge/Fortran-%23734F96.svg?style=for-the-badge&logo=fortran&logoColor=white)
[![fpm](https://img.shields.io/badge/fpm-Fortran_package_manager-734f96)](https://fpm.fortran-lang.org)

A Fortran 2008 library for Self-Organizing Maps using object-oriented programming and modern Fortran features.

## Motivation

Self-Organizing Maps or Kohonen Maps are powerful computational tools to cluster multivariate data using a topology preservation approach, that is, the clustering obtained by using this methodology is designed to preserve neighboring relationships between samples (closer samples in the input space remain closer in the output space).

Two Level Self-Organizing Maps are important in many applied areas but currently there is not a proper implementation that suited my needs. So I decided to implement this clustering approach in Fortran using the features included in the new standard Fortran2008. This ensures computational efficiency and software extensibility, in addition to having to learn more about Fortran.


## Get the code 

Close the repository
```
git clone  https://github.com/khaors/KohonenF08
cd KohonenF08
```


## Compilation with fpm

The Fortran Package Manager (fpm) can be used to build `KohonenF08` using the provided `fpm.toml` file:


```
fpm build --profile release
```

## Testing

The test can be executed using:

```
fpm test --profile release
```

## Parameter File Format

### SOM_TRAIN

The structure of the parameter file of the _som_train_ program is the following:

```
!
! Parameter file to the som_train program
!
! Here you can specify all the information related with this specific run of the program such as date and reason 
! why you run the program in first place.
! 
!
SOM_TRAIN_PARAMETERS
0                                                 !SOM OPTION
iris.dat                                          !FILE WITH INPUT PATTERNS
150                                               !NUMBER OF PATTERNS
4                                                 !NUMBER INPUT VARIABLES
1 2 3 4                                           !COLUMNS
normal_som                                        !SOM_TYPE (normal_som,visom)
10 10 1                                           !NUMBER_NODES IN SOM (NODES IN X, Y AND Z)
200                                               !NUMBER EPOCHS
0.1                                               !LEARNING RATE
12345                                             !RANDOM SEED
euclidean                                         !DISTANCE TYPE(euclidean is the only option)
rectangular                                       !NODE TYPE(rectangular,hexagonal)
gaussian                                          !NEIGHBORHOOD TYPE(bubble,gaussian)
0                                                 !DEBUG LEVEL
som_iris.dbg                                      !DEBUG FILE
som_iris                                          !BASE NAME FOR THE OUTPUT FILES
1                                                 !TOROIDAL GRID(0=NO, 1=YES)
```

Some explanation of the parameters is included in the following lines:

- SOM OPTION: 
- SOM_TYPE: This refers to the type of SOM used in the analysis. Currently there are only two options: normal_som refers to the conventional SOM where the difference between the distance between the samples and the prototypes that define the map is minimized. visom is the  Visualisation induced Self-Organising Map (ViSOM) proposed by Yin and is equivalent to a 2D PCA of the original dataset. More details about this approach can be obtained [here](https://personalpages.manchester.ac.uk/staff/hujun.yin/mypublications/preprint-visom.pdf).
- NUMBER NODES IN SOM: These three numbers define the size of the map where the high-dimensional information is projected. Only 2D projections is supported in these programs.
- NODE TYPE: Maybe this is not the best word to describe this property but this refers to the topology of the map used in the 2D projection of the high-dimensional data. Currently two options are implemented: rectangular and hexagonal grids. The rectangular grid is the easiest to implement but it might induce some artifacts in the 2D representation due its rigid structure and therefore the hexagonal grid is always recommended. 
- NEIGHBORHOOD TYPE: This option defines the region in the map that is modified when a sample is presented to the map. The simples option is _bubble_ that represents a squared region around the BMU (Best Matching Unit) with a defined size. The _gaussian_ neighborhood is the recommended option.
- DEBUG LEVEL AND DEBUG FILE: This option is only for developers and a debug level greater than 0 prints more information about the inner workings of the map including reading the parameters, calculating the distortions, the weight updates and final results.
- BASE NAME FOR THE OUTPUT FILES: This is the base of the name of the files where all results are going to be saved. This program produces several output files:
    + Distortion file: This file includes the value of the distortion at each epoch during the training phase
    + Map_samples file: This file contains that ID of the samples associated to each node of the 2D of the SOM
    + Neuron_distances file: This file contains the distance matrix between the prototype that defines each neuron of the SOM.
    + Neuron_hit file: This file contains the number of input samples associated to each one of the neurons of the SOM.
    + Prototype file: This file includes the values of the vectors that define the prototypes of each neuron of a SOM. These values can be interpreted as the centers of the clusters that represents the original data.
    + U_matrix file: This file contains the U matrix of the SOM. More information about this important SOM result can be obtained [here](https://en.wikipedia.org/wiki/U-matrix)  
- TOROIDAL GRID: If 1 then the sides of the map are connected in such a way that the 2D projection is done over a grid with donut-like shape. This avoids border effects that can induce artifacts in the clustering of high-dimensional datasets. 


### TWO_LEVEL_SOM_TRAIN

The Two Level SOM is an architecture where two SOM are used to cluster the input data. The first level is a conventional SOM where the input data is projected into a 2D grid and the second level is a SOM where the prototypes identified in the first level are projected into a 1D that is defined in terms of the number of clusters that need to be identified. More information about this approach can be obtained [here](https://ieeexplore.ieee.org/document/846731). 

The parameter file of the _two_level_som_train_ program has the following structure:

```
!
!
!
TWO_LEVEL_SOM_TRAIN_PARAMETERS
LAYER1
0                                                 !TRAINING OPTION
iris.dat                                          !INPUT PATTERNS FILE
150                                               !NUMBER PATTERNS
4                                                 !NUMBER INPUT VARIABLES
1 2 3 4                                           !COLUMNS
normal_som                                        !SOM_TYPE (normal_som,visom)
10 10 1                                           !NUMBER_NODES IN SOM (NODES IN X, Y AND Z)
200                                               !NUMBER EPOCHS
0.1                                               !LEARNING RATE
12345                                             !RANDOM SEED
euclidean                                         !DISTANCE TYPE(euclidean is the only option)
rectangular                                       !NODE TYPE(rectangular,hexagonal)
gaussian                                          !NEIGHBORHOOD TYPE(bubble,gaussian)
0                                                 !DEBUG LEVEL
som_iris2l.dbg                                    !DEBUG FILE
som_iris2l                                        !BASE NAME FOR THE OUTPUT FILES
1                                                 !PRINT RESULTS
1                                                 !TOROIDAL GRID(0=NO, 1=YES)
LAYER2
5 1 1                                             !NUMBER NODES
100                                               !NUMBER EPOCHS
0.1                                               !LEARNING RATE
12346                                             !RANDOM SEED
euclidean                                         !DISTANCE TYPE
rectangular                                       !NODE TYPE
gaussian                                          !NEIGHBORHOOD TYPE

```

The options are a direct extension of the _som_train_ program and these are explained in the previous section. 

## Testing

A simple example is included in the repository in which the goal is to find the clusters present in the _iris_ dataset. This dataset is included in the _iris.dat_ file where the samples have been properly scaled in the range between 0 and 1. The input parameters of the program _som_train_ are specified in a parameter file as explained in the previous section. The clustering of the _iris dataset_ can be done writing on the command line:

```
som_train som_train.par
```

```
two_level_som_train two_level_som_train.par
```


## Documentation

The documentation of the library can be accessed in the following links:

- [html files](https://khaors.github.io/KohonenF03/index.html)


## Licensing, Authors, Acknowledgements
KohonenF08 was written by Oscar Garcia-Cabrejo and is distributed under the [MIT license](https://github.com/khaors/KohonenF08/blob/master/LICENSE). 

You can cite KohonenF08 in research publications and reports as follows:
* Garcia-Cabrejo, O. (2025). ***KohonenF08: A library for Self-Organizing Maps in Fortran 2008***. https://github.com/khaors/KohonenF08. Accessed: *day month year*.

BibTeX entry:
```
@misc{Garcia-Cabrejo25,
 author = {Garcia-Cabrejo, O},
 title 	= {{KohonenF03: A library for Self-Organizing Maps in Fortran 2008}},
 year 	= 2025,
 howpublished = {\url{https://github.com/khaors/KohonenF03}},
 note 	= {Accessed: day month year}
}
```

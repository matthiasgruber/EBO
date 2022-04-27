# EBO - a material science toolbox

An R package designed for material science. EBO is connected to the black-box function optimization R packages mlrMBO, SPOT, iRace, and cmaesr. EBO has outstanding benchmark features, specifically for model-based optimization.

# Project goal

In material sciences, one often faces the challenge of optimizing black-box functions with an input-output relation. Input parameters can be chemical structure, composition, or processing conditions, and the quality of the resulting material defines the output. As the evaluation of such experiments is labor-intensive, time-consuming, and related to high costs, we are interested in achieving the best results with the lowest number of evaluations.  

While human experts developed and explored these parameter settings based on literature or chemical and physical laws in the past, machine learning nowadays can achieve better results than human experts. The general model-based optimization approach simulates input-output relation with so-called surrogate models. After that, the next promising points are proposed with an iterative process. This procedure enables researchers to improve their choices of which parameter settings to test next.  

Which optimization algorithm should a material scientist choose?

# Solution

The solution in this project was an R package named EBO. EBO is connected to the black-box function optimization packages mlrMBO, SPOT, iRace, and cmaesr. EBO has outstanding benchmark features, specifically for model-based optimization. Further, it is possible to tune the parameters of the model-based optimization algorithm automatically.




# Material-Science and Machine Learning

Simulation and optimization algorithms enjoy a great popularity in many fields, such as aerospace, material-science and engineering, just to name a few examples. Especially in material-science, machine learning contains a wide range of applications. With machine learning it was possible to discover new solar light absorbers [Bei et al., 2018] as well as new metallic glasses [Ren et al., 2018] or to optimize the fabrication of laser-induced graphene [Kottho� et al., 2019]. In material sciences, one often faces the challenge of optimizing black-box functions with an input-output relation. Input parameters can be chemical structure, composition, or processing conditions, and the quality of the resulting material defines the output. As the evaluation of such experiments is labor-intensive, time-consuming, and related to high costs, we are interested in achieving the best results with the lowest number of evaluations. While human experts developed and explored these parameter settings based on literature or on chemical and physical laws in the past, machine learning nowadays is able to achieve better results than human experts. The general approach of model-based optimization is the simulation of input-output relation with so called surrogate models. Thereafter, the next promising points are proposed with an iterative model-based approach. This procedure enables researchers to improve their choices which parameter settings to test next.

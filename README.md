# EBO - an R package designed for material science

# Project goal

Simulation and optimization algorithms are popular in fields, such as aerospace, material-science and engineering. Especially in material-science, machine learning can be apllied to discover new solar light absorbers [Bei et al., 2018] as well as new metallic glasses [Ren et al., 2018] or to optimize the fabrication of laser-induced graphene [Kotthoff et al., 2019]. The general approach is optimizing black-box functions with an input-output relation. Input parameters can be chemical structure, composition, or processing conditions, and the quality of the resulting material defines the output. As the evaluation of such experiments is labor-intensive, time-consuming, and related to high costs, we are interested in achieving the best results with the lowest number of evaluations.  

While human experts developed and explored these parameter settings based on literature or chemical and physical laws in the past, machine learning nowadays can achieve better results than human experts. The general model-based optimization approach simulates input-output relation with so-called surrogate models. After that, the next promising points are proposed with an iterative process. This procedure enables researchers to improve their choices of which parameter settings to test next.  

Which optimization algorithm should a material scientist choose?

# Solution

The solution in this project was an R package named EBO. EBO is connected to the black-box function optimization packages mlrMBO, SPOT, iRace, and cmaesr. EBO has outstanding benchmark features, specifically for model-based optimization. Further, it is possible to tune the parameters of the model-based optimization algorithm automatically.

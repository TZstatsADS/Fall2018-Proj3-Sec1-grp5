# Super Resolution CNN

## Usage

1. Please use jupyter notebook to run `prepare_data.ipynb` first, and produce `crop_train.h5` file which is around 8.5 GB.
2. Adjust the dataset path
3. Run the `model.ipynb` to train the model and predict test set.

## Required packages

```
$pip install opencv-python
$pip install tensorflow
$pip install keras
$pip install h5py

```

## Results
* Results are in `./output/` folder

* `hr.jpg` is the original HR image
* `resized.jpg` is the LR image but resized to the same size as HR image
* `test.jpg` is the result after super resolution.
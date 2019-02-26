# tbgeneratr 0.1.1.2
#### 25 Feb 2019
## Minor update
* `age_generator()`: add `categorise` argument to generate additional factor variable for age. 
* `age_generator()`: add `paediatric` argument to generate additional paediatric age variable.
* args fixed in `age_generator.default()` 

---

# tbgeneratr 0.1.1.1
#### 12 Feb 2019
## Bug fix
* `baseliner.koch6()`: fixed bug of object class being erased by function. 

---

# tbgeneratr 0.1.1.0
#### 04 Feb 2019
## Major changes
* `drug_timer()` added for Koch6 and EpiInfo data sets. This function is designed to calculate
the number of days a patient has been prescribed a specified drug in their treatment regimen. It
 requires admission and change data sets both cleaned using the `tbcleanr` package. 

---

# tbgeneratr 0.1.0.0
#### 17 Jan 2019
## Major changes
* release 0.1.0.0 which has functionality for Koch6 and EpiInfo but not Koch6 laboratory data

---

# tbgeneratr 0.0.0.21.9003
#### 17 Jan 2019
## Major changes
* `aggregate_dst()` added to simplify `dst_baseliner()`.
* `dst_baseliner.epiinfo()` added to package.
* `dst_baseliner.koch6()` added to package - option for Gr and EpiInfo lab data.

## Minor changes
* `drug_baseliner()` args changed.
* `drug_baseliner()` naming output bug fixed. 

---

# tbgeneratr 0.0.0.21.9002
#### 12 Jan 2019
## Minor changes
* Added a `NEWS.md` file to track changes to the package.

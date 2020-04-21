LOAD R

SET WORKING DIRECTORY TO WHERE CODE IS STORED
nohup Rscript 'fp1.1_female_dataets_loaded_sample_size.R' > 'fp1.1_female_dataets_loaded_sample_size.out'
nohup Rscript 'fp1.2_generate_risks_calibration_population_female.R' > 'fp1.2_generate_risks_calibration_population_female.out'
nohup Rscript 'fp2.1_generate_risks_different_cohort_sizes_female_1434.R' > 'fp2.1_generate_risks_different_cohort_sizes_female_1434.out' 
nohup Rscript 'fp2.2_generate_risks_different_cohort_sizes_female_10000.R' > 'fp2.2_generate_risks_different_cohort_sizes_female_10000.out'
nohup Rscript 'fp2.3_generate_risks_different_cohort_sizes_female_50000.R' > 'fp2.3_generate_risks_different_cohort_sizes_female_50000.out'
nohup Rscript 'fp2.4_generate_risks_different_cohort_sizes_female_100000.R' > 'fp2.4_generate_risks_different_cohort_sizes_female_100000.out'
nohup Rscript 'fp3.1_Figure2_female_CI_boxplot_by_sample_size.R' > fp3.1_Figure2_female_CI_boxplot_by_sample_size.out'
nohup Rscript 'fp3.2_Figure3_female_CI_boxplot_by_C_statistics_1434.R' > 'fp3.2_Figure3_female_CI_boxplot_by_C_statistics_1434.out'
nohup Rscript 'fp3.2_Figure3_female_CI_boxplot_by_C_statistics_10000.R' > 'fp3.2_Figure3_female_CI_boxplot_by_C_statistics_10000.out'
nohup Rscript 'fp3.2_Figure3_female_CI_boxplot_by_C_statistics_50000.R' > 'fp3.2_Figure3_female_CI_boxplot_by_C_statistics_50000.out'
nohup Rscript 'fp3.2_Figure3_female_CI_boxplot_by_C_statistics_100000.R' > 'fp3.2_Figure3_female_CI_boxplot_by_C_statistics_100000.out'
nohup Rscript 'fp3.3_Figure4_female_CI_boxplot_by_calibration_centered_1434.R' > 'fp3.3_Figure4_female_CI_boxplot_by_calibration_centered_1434.out'
nohup Rscript 'fp3.3_Figure4_female_CI_boxplot_by_calibration_centered_10000.R' > 'fp3.3_Figure4_female_CI_boxplot_by_calibration_centered_10000.out'
nohup Rscript 'fp3.3_Figure4_female_CI_boxplot_by_calibration_centered_50000.R' > 'fp3.3_Figure4_female_CI_boxplot_by_calibration_centered_50000.out'
nohup Rscript 'fp3.3_Figure4_female_CI_boxplot_by_calibration_centered_100000.R' > 'fp3.3_Figure4_female_CI_boxplot_by_calibration_centered_100000.out' 
nohup Rscript 'fp3.4_Table3_female_percentage_over_threshold.R' > 'fp3.4_Table3_female_percentage_over_threshold.out'
nohup Rscript 'fp3.5_baseline_table_female.R' > nohup Rscript 'fp3.5_baseline_table_female.out' >
nohup Rscript 'fp3.6_Table2_quantiles.R' > 'fp3.6_Table2_quantiles.out' 

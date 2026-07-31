#!/bin/bash
set -o pipefail
set -o errexit

#export PATH="/opt/conda/bin:/opt/miniconda/bin:/usr/local/bin:/usr/bin:/bin:$PATH"

# Restricted to chromosome 22 based on your file tree
chr_list=(22)

# Test Parameters to run above
ld_windows=(250)
r2_values=(0.2)
pvals=(5e-2)
phi_list=(auto)

# Create logs directory
mkdir -p logs

# Map input and output paths to standard Code Ocean directories based on your image tree
bfile_prefix="/data/Genotypes/geno_chr"
clump_out_dir="/results/C_T/"
mkdir -p "$clump_out_dir"

# Define base directories for Code Ocean environment
ref_dir="/data/LD_reference/"
bim_prefix="/data/Genotypes/geno_chr"
sumstats_dir="/data/Summary_Statistics"

# ==========================================
# 1. Clumping Analysis (C + T) Sweep
# ==========================================
for chr_num in "${chr_list[@]}"; do
  for ld_window in "${ld_windows[@]}"; do
    for r2 in "${r2_values[@]}"; do
      for pval in "${pvals[@]}"; do

        r2_label=$(echo $r2 | sed 's/\./p/')
        pval_label=$(echo $pval | sed 's/e/-e/; s/\./p/')

        echo "Processing Clumping for chr${chr_num} LDwin${ld_window} R2${r2} p${pval}"

        "${plink}" \
          --bfile "${bfile_prefix}${chr_num}_updated" \
          --clump "${sumstats_dir}/meta_mvp_meta_bbj_v1_07242026.txt" \
          --clump-p1 "${pval}" \
          --clump-r2 "${r2}" \
          --clump-kb "${ld_window}" \
          --out "${clump_out_dir}/meta_all_hcm_eas_chr${chr_num}_ld${ld_window}_r2${r2_label}_p${pval_label}"

      done
    done
  done
done

# ==========================================
# 2. PRScsx.py Execution across all Phi Values
# ==========================================

chr_list=("22")
phi_list=("auto" "1e-2")

for chr_num in "${chr_list[@]}"; do
  chr_num=$(echo "${chr_num}" | sed 's/chr//g')

  for phi in "${phi_list[@]}"; do
    if [[ "$phi" == "auto" ]]; then
      phi_label="auto"
      PHI_ARG=""
    else
      phi_label=$(echo $phi | sed 's/\./p/')
      PHI_ARG="--phi=${phi}"
    fi

    out_dir="/results/prscsx_harmonized/phi_${phi_label}"
    mkdir -p "$out_dir"

    /usr/bin/python3 /code/PRScsx/PRScsx.py \
      --ref_dir="${ref_dir}" \
      --bim_prefix="${bim_prefix}${chr_num}_updated" \
      --sst_file="${sumstats_dir}/GCST90435254_07242026.txt,${sumstats_dir}/META_BBJ_07242026.txt,${sumstats_dir}/MVP_HOCM_07242026.txt,${sumstats_dir}/MVP_NHCM_07242026.txt" \
      --n_gwas="74259,178128,572697,572656" \
      --pop="EUR,EAS,EUR,EUR" \
      --chrom="${chr_num}" \
      ${PHI_ARG} \
      --out_name="hcm_chr${chr_num}_phi${phi_label}" \
      --out_dir="${out_dir}" \
      --meta=true \
      --seed=12345
  done
done

# ==========================================
# 3. Polygenic Risk Score (PRS) Generation (PLINK)
# ==========================================

score_out_dir="/results/prs_scores"
mkdir -p "$score_out_dir"

for phi in "${phi_list[@]}"; do
  if [[ "$phi" == "auto" ]]; then
    phi_label="auto"
  else
    phi_label=$(echo $phi | sed 's/\./p/')
  fi

  # PRScsx outputs meta-analyzed effect sizes when --meta=true
  # File pattern format: <out_dir>/<out_name>_<pop>_pst_eff_a1_b0_phi<phi>_chr<chrom>.txt (or meta-analyzed equivalent)
  # Assuming standard naming convention for meta-analyzed output files:
  prs_input_file="/results/prscsx_harmonized/phi_${phi_label}/hcm_chr22_phi${phi_label}_meta_pst_eff_a1_b0_phi${phi_label}_chr22.txt"

  if [[ -f "$prs_input_file" ]]; then
    echo "Calculating PRS for phi=${phi_label} using PLINK..."

    plink \
      --bfile "${bfile_prefix}22_updated" \
      --score "$prs_input_file" 2 4 6 header \
      --out "${score_out_dir}/hcm_chr22_phi${phi_label}_prs"
  else
    echo "Warning: Expected PRS-CSx output file $prs_input_file not found. Skipping score generation for phi=${phi_label}."
  fi
done
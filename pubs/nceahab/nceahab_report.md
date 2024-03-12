---
title: Assessing the cumulative effects of human activities and climate change on habitats and species of the Scotian Shelf
author: David Beauchesne^1,2\*^, Kevin Cazelles^1,3^, Steve Vissault^1^
fontsize: 12pt
output:
  pdf_document:
    toc: false
    number_sections: false
header-includes:
   - \usepackage{lineno}
   - \linenumbers
   - \usepackage{listings}
   - \usepackage{float}
   - \usepackage{setspace}
   - \usepackage{longtable}
   - \usepackage{booktabs}
   - \usepackage{tabularx}
   - \righthyphenmin=62
   - \lefthyphenmin=62
   - \usepackage{natbib}
bibliography: [pipedat.bib, nceadfo.bib, nceahab.bib]
csl: frontiers.csl
link-citations: yes
relativeurls: true
---

<!--
rmarkdown::render('./nceahab_report.md')
rmarkdown::render('./nceahab_report.md', output_format = "word_document")
-->

**Affiliations:**

^1^inSileco Inc., Québec, Canada

^2^Department of Health and Society, University of Toronto; Toronto, Canada.

^3^Department of Integrative Biology, University Of Guelph; Guelph, Canada.

\doublespacing

<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->
<!-- 

Plan: 
- Context
- Method
  - Models
  - Data
- Results
- Bibliography

-->
<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->

<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->
<!-- # Abstract -->
<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->


<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->
# Context
<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->

Effective management and mitigation of multiple human impacts on marine ecosystems require accurate knowledge of the spatial patterns of human activities, and their overlap with vulnerable habitats and species. Despite broad recognition of their importance and likelihood of occurrence, consideration of the cumulative effects of anthropogenic stressors (e.g., habitat loss, invasive species, climate change) to the recovery of species and habitats remains a complex issue. As a result, cumulative interactions among identified stressors, which stressors are most impactful, and how they may be effectively mitigated, often remain unknown. To date, models of cumulative effects on marine ecosystems estimate impacts on species or habitats separately; their connection within the same spatial model has not yet been attempted. Recently, two independent analyses performed for the Scotian Shelf sought to assess cumulative effects to species [@beauchesne2023a] and habitats [@murphy2023], respectively. The goal of this project is to combine both assessment into an ecosystem-scale cumulative effects assessment that considers direct effects to habitats and species as well as indirect effects to species arising from species interactions and habitat use. This project will be the first application of a joint species-habitat cumulative effects assessment and represents a novel ecosystem-level approach to estimate cumulative impacts of human activities and climate change over large spatial scales. 

More specifically, the deliverables of the current project were the following:

1. Ecosystem-scale cumulative effects assessment combining species-scale and habitat-scale assessments.
   - Raw results of @halpern2008a and @beauchesne2023a methods
   - Geographical overlap between habitats and species distribution
   - Weighting for indirect effects to species arising from habitat use
2. Spatial results figures 
   - Cumulative effects on species 
   - Cumulative effects on habitats
   - Ecosystem-scale cumulative effects
3. Non-spatial results figures
   - Contribution of stressors to:
     - Direct effects to habitats
     - Direct effects to species
     - Indirect effects to species arising from species interactions 
     - Indirect effects to species arising from habitats used
  - Multiplex figure of cumulative effects including:
     - Subnetworks
        - Stressors
        - Species 
        - Habitats 
      - Links
        - Stressors and species
        - Stressors and habitats 
        - Species and species (*i.e.* species interactions)
        - Species and habitats
4. Written summary of methods and results 

 
<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->
<!-- # Introduction -->
<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->



<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->
# Materials and Methods
<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->

## Habitat-scale cumulative effects assessment model

The habitat-scale assessment developed by @halpern2008a is a spatially explicit method that predicts relative cumulative effects on species for a set of stressors. Considering a study area divided into cells of homogeneous size, in a given cell $x$, the effect of a stressor $j$ on habitat $i$ is the product of three components: 1) the mapped presence or absence of habitat $i$, $H_{i,x}$; 2) the spatial distribution and relative intensity of environmental stressors $j$, $D_{j,x}$; 3) the habitat-specific sensitivity of habitat $i$ to stressor $j$ ($\mu_{i,j}$). The cumulative effect in $x$, $C_{H_x}$, is then computed as the sum of all products, i.e. the sum of the effect of all considered stressors on the entire set of habitats:

$$C_{H_x} = \sum_i \sum_j H_{i,x} D_{j,x} \mu_{i,j}$$

where $D_j$ is the log-transformed and scaled. This method predicts relative cumulative effects that incorporates knowledge on habitat distribution, their exposure to environmental stressors, and their known or suspected sensitivity to the effects of stressors. The data modules used for the Scotian Shelf Bioregion habitat-scale cumulative effects assessment are described in @murphy2023. For this analysis, were subsampled the environmental stressors used in the assessment to match those used in the network-scale assessment of the Scotian Shelf Bioregion (see @beauchesne2023b). This results in an assessment of the effects of 17 stressors on 21 habitats in the Scotian Shelf Bioregion. 


## Network-scale cumulative effects assessment model

The approach developed by @beauchesne2023a builds on the method developed by @halpern2008a and combines it with recent progress in theoretical ecology [@stouffer2007; @stouffer2012; @beauchesne2021] to predict the net effects of environmental drivers by considering both direct and trophically-mediated indirect effects in ecological communities. By focusing on interactions rather than individual species, this approach provides the ability to consider how a focal species is affected by multiple environmental pressures, but also integrates how species it interacts will respond to the same pressures and how their response propagates to the species of interest.  

The method proposed by @beauchesne2023a decomposes food webs into collections of $p$-species interactions called motifs [@milo2002]. Motifs that provide a mesoscale characterization of the structural properties of ecological networks [@bascompte2005; @stouffer2007; @stouffer2010; @stouffer2011; @bramonmora2018]. In a $n$-species food web ($n \geq p$), the collection of $p$-species motifs ($p \leq n$) in which species $k$ is involved in ($M_k = \{m_{k,1},m_{k,2},...,m_{k,x}\}$) forms its motif census ($M_k$) [@stouffer2012; @beauchesne2021]. The motif census provides an overview of all the interactions and connected species likely to affect a species’ dynamics, and the propagation of disturbances through their interactions. 

The network-scale model focuses on the most abundant 3-species motifs found in empirical food webs (*i.e.* trophic food chain, omnivory, exploitative and apparent competition) [@camacho2007; @stouffer2010] to assess a species motif census. Network-scale cumulative effects scores ($C_N$) are predicted in each grid cell $x$ as follows: 

$$C_{N_x} = \sum_k \frac{1}{|M_k|} \sum_{m_{k,x} \in M_k} \sum_j D_j \overline{\mu_j} T_{k_{m_{k,x}}}$$

where $k$ is the focal species, $M_k$ is the motif census of species $k$, $m_{k,x}$ are the 3-species motifs of interest forming species $k$'s motifs census, and $D_j$ is the log-transformed and scaled intensity of stressors $j$. 

$\overline{\mu_j}$ is the joint sensitivity of species in motif $m_{i,x}$ to stressor $j$. This joint sensitivity considers that a species' response to stressors integrates its own response as well as the response of species it interacts with. The joint sensitivity is measured as:

$$\overline{\mu_j} = w_1 \mu_{k,j} + w_2 \sum_l^2 \mu_{l,j}$$

where $\mu_{k,j}$ and $\mu_{l,j}$ are the sensitivities to stressor $j$ of focal species $k$ and of the two species interacting with species $k$ in motif $m_{k,x}$, respectively. 

$w_1$ and $w_2$ are weighting factors used to give a relative importance to direct -- *i.e.* effects to species $k$ -- and indirect -- *i.e.* effects propagating through species $l$ to species $k$ -- effects in the assessment. $w_1 + 2 w_2 = 1$ so that weighting provides a percent contribution to direct and indirect effects. Here, we used $w1 = 0.5$ and $w2 = 0.25$ under the assumption that direct and indirect would have equal weights. 

$T_{k_{m_{k,x}}}$ is the trophic sensitivity of species $k$ in motif $m_{k,x}$. The trophic sensitivity of a species relates to a species propensity to be affected by trophically-mediated indirect effects. This sensitivity is related to the structure of the community, a species' trophic position, and the pathways of effects through which stressors are indirectly propagating to a species [@beauchesne2021]. 

In terms of data requirements, this network-scale assessment builds on the same 3 modules as @halpern2008a, *i.e.* 1) the [distribution of species](https://ecosystem-assessments.github.io/nceadfo/report/modules.html#spmodule) ($S_k$), 2) the [normalized distribution and intensity of environmental drivers](https://ecosystem-assessments.github.io/nceadfo/report/modules.html#drmodule) ($D_j$), and 3) the [species-specific sensitivity of each species to each driver](https://ecosystem-assessments.github.io/nceadfo/report/modules.html#sssmodule) ($\mu_{k,j}$); it considers 2 additional modules to complete the assessment: 4) the [metaweb of species interactions](https://ecosystem-assessments.github.io/nceadfo/report/modules.html#metamodule), *i.e.* the network of binary biotic interactions structuring local food webs, and 5) the [trophic sensitivity of species](https://ecosystem-assessments.github.io/nceadfo/report/modules.html#trsmodule) ($T_i$), *i.e.* their sensitivity to trophically-mediated indirect effects (see below for more details). These data modules are described in detail in @beauchesne2023b for the Scotian Shelf species-scale assessment, and links in this text lead to relevant sections from that report. The data was used to perform an assessment of the effects of 17 stressors on 205 species of the Scotian Shelf Bioregion.

## Indirect effect to species through habitats

The network-scale assessment considers that indirect effects propagate from one species to another through their ecological interactions. Similarly, it is expected that indirect effects of stressors will propagate to species from effects to their habitats. Indirect effects to species arising from habitats were assessed as a function of cumulative effects to individual habitats and species habitat use in each grid cell $x$ as follows: 

$$I_{k,x} = \frac{1}{\sum_{i} H_{i,x} U_{k,i}} \sum_i H_{i,x} U_{k,i} \sum_j D_{j,x} \mu_{i,j}$$

where $k$ is the focal species, $i$ is a habitat, $j$ is a stressor, $H_{i,x}$ is the mapped presence or absence of habitat $i$, $D_j$ is the log-transformed and scaled intensity of stressor $j$, and $\mu_{i,j}$ is the habitat-specific sensitivity of each habitat to each stressor. $U_{k,i}$ is the affinity of species $k$ to habitat $i$ (more on this below). Since multiple habitats may be found in a single cell, we scale the effects predicted by $\sum{i} H_{i,x} U_{k,i}$ to account for the number of habitats used in a single cell. Note that $H_{i,x}$ could be an area for each habitat within each cell; we would then scale the predicted effects the the percent area used by each species in cell $x$. 

Species are not uniformly distributed among different habitat types and they may have greater or lower affinity for different types of habitats. Furthermore, species and habitats are distributed in 3-dimension in marine environment, which is not captured by the 2-dimensional spatial data used for the assessment. This means, for example, that a pelagic species such as sea birds could be present in a grid cell where benthic habitats are also present without using the benthic habitats in actuality. To account for the 3-dimensional distribution of habitats and species, we used environmental traits to match which habitats may be used by which species. 

The environment in which species are found was extracted from the World Register of Marine Species (WoRMS) [@wormseditorialboard2017], FishBase [@froese2019], SeaLifeBase [@palomares2019], and the Encyclopedia of Life [@encyclopediaoflife2020] (see @beauchesne2023a for more details). Species were characterized either as bathydemersal (*i.e.* living and/or feeding on or near the bottom) and/or pelagic (*i.e.* occurring mainly in the water column) environments; species could span both types of habitats. 

Habitats, meanwhile, were characterized using the same categories. Habitats considered as spanning both bathydemersal and pelagic environments were the tidal zones (0-2m), nearshore habitats (2-30m), saltmarshes, seagrass beds, algal zones, kelp forests, and horse mussel bioherms. Habitats considered as only bathydemersal were shelf habitats (30-200m), bathyal habitats, canyons, and deep biogenic habitats. Habitat considered as only pelagic were pelagic habitats. 

We used this categorization for species and habitats to assess which habitats may be used by which species in each grid cell. Indirect effects predicted from habitats with a 3-dimensional mismatch in use were therefore set to zero in the assessment of the indirect effects to species arising through effects to habitats. 


## Joint cumulative effects to species 

Results from the network-scale assessment (*i.e.* direct stressors effects and indirect effects through species interactions) are then combined with the indirect effects to species through habitats. As the units from these assessments cannot be compared, we scale both assessments before combining them together. Results from the network-scale assessment were normalized between 0 and 2 to account for the two types of effects considered in that assessment, while indirect effects to species through habitats were normalized between 0 and 1. Results from both assessments were then summed within each grid cell to provide a joint cumulative effects assessment to species that considers direct stressors effects and indirect effects arising through species interactions and habitats. 

## Ecosystem-scale cumulative effects assessment

Finally, the ecosystem-scale cumulative effects assessment combines results from the joint cumulative effects to species with the habitat-scale assessment in order to provide an overview of cumulative risk to species and habitats together, where each is considered as a subnetwork. As with the joint assessment, units cannot be compared between both assessments. Both sets of results were therefore normalized between 0 and 1 before being combined into a single assessment. We also provide cumulative effects results scaled by the number of species (*i.e.* $n = 205$) or habitats (*i.e.* $n = 21$) within each subnetwork. 


## Assessment and spatial data representation

The open-source software R 4.2.3 was used for all analyses [@rcoreteam2023] and the package *rcea* [@beauchesne2023] was used to perform the cumulative effects assessment. See table S7 for a list of all R packages used. All datasets are presented at a 1 $km^2$ resolution even though some source data had coarser resolutions (table S1). We resampled and reprojected data when necessary using nearest neighbour estimates, which preserves the values of the source data. By doing so, we assume that the coarser data are evenly distributed across finer-scale cells with which they overlap. We used the NAD83 / Quebec Lambert projection (EPSG: 32198), which is well suited to represent and preserve surface area within our study system. 


<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->
# Results
<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->

## Network-scale and habitat-scale assessments 

\begin{figure}[H]
\centering
\includegraphics{./figures/ncea_ceahab.png}
\caption{\textbf{Network-scale and habitat-scale cumulative effects assessments.} Network-scale cumulative effects assessment of 17 stressors on 205 species in the Scotian Shelf Bioregion in eastern Canada explicitly considering the underlying structure of the ecological community (\textbf{A}). Habitats-scale cumulative effects assessment of 17 stressors on 21 marine habitats of the Scotian Shelf Bioregion in eastern Canada (\textbf{B}).}
\label{ncea_ceahab}
\end{figure}
\newpage


## Joint cumulative effects on species

\begin{figure}[H]
\centering
\includegraphics{./figures/nceahab_species.png}
\caption{\textbf{Joint cumulative effects assessment.} Joint cumulative effects assessment of 17 stressors and on 205 species of the Scotian Shelf Bioregion in eastern Canada (\textbf{A}) taking into consideration direct effects of stressors (\textbf{B}), indirect effects propagating through species interactions (\textbf{C}), and indirect effects arising through cumulative effects to the habitats used by species (\textbf{D}).}
\label{joint}
\end{figure}
\newpage


## Ecosystem-scale cumulative effects assessment

\begin{figure}[H]
\centering
\includegraphics{./figures/nceahab.png}
\caption{\textbf{Ecosystem-scale cumulative effects assessment.} Ecosystem-scale assessment of cumulative effects of 17 stressors on 21 marine habitats and 205 species of the Scotian Shelf Bioregion in eastern Canada using untransformed cumulative effects scores (\textbf{A}) and scores normalized by the number of habitats and species, respectively (\textbf{B}).}
\label{ecosystem}
\end{figure}
\newpage

## Metaweb

\begin{figure}[H]
\centering
\includegraphics{./figures/metanetwork.png}
\caption{Multiplex network presenting the presence of an effect of individual stressors on each species and each habitat (coloured links), indirect effects arising from ecological interactions (grey links), the indirect effects arising from species habitat use (blue links), the overall cumulative effect on each species and habitats (species and habitat point sizes) and the mean effect of stressors (stressor point sizes).}
\label{metaweb}
\end{figure}
\newpage

## Contribution to cumulative effects

\begin{figure}[H]
\centering
\includegraphics{./figures/contribution_nceahab-2016_2021.png}
\caption{
   \textbf{Contribution to ecosystem-scale cumulative effects} Mean contribution of 17 stressors to the cumulative effects ($C / km^2$) of 205 species and 21 marine habitats of the Scotian Shelf Bioregion in eastern Canada. Taxonomic groups for invertebrates and vertebrates are grouped at the Phylum and Class levels, respectively, for ease of representation and interpretation. The mean contribution for each species and habitat was used to evaluate each stressor's contribution to overall cumulative effects. For habitats, only direct effects of stressors are presented. For species, effects are divided into direct and indirect effects. Direct effects are those attributable to the effect of a stressor on a focal species, while indirect effects are the mean effects of stressors spreading through all 3-species interactions a taxon is involved in, and the mean effect to habitats used by the focal species. The total contribution of each driver to cumulative effect is the sum of their direct and indirect contributions to cumulative effects.}
\label{contribution}
\end{figure}
\newpage



<!-- =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= -->
# References
<!-- =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= -->





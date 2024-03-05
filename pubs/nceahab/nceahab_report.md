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

Effective management and mitigation of multiple human impacts on marine ecosystems requires accurate knowledge of the spatial patterns of human activities, and their overlap with vulnerable habitats and species. Despite broad recognition of their importance and likelihood of occurrence, consideration of the cumulative effects of anthropogenic stressors (e.g., habitat loss, invasive species, climate change) to the recovery of species and habitats remains a complex issue. As a result, cumulative interactions among identified stressors, which stressors are most impactful, and how they may be effectively mitigated, often remain unknown. To date, models of cumulative effects on marine ecosystems estimate impacts on species or habitats separately; their connection within the same spatial model has not yet been attempted. Recently, two independent analyses performed for the Scotian Shelf seeked to assess cumulative effects to species [@beauchesne2023a] and habitats [@murphy2023], respectively. The goal of this project is to combine both assessment into an ecosystem-scale cumulative effects assessment that considers direct effects to habitats and species as well as indirect effects to species arising from species interactions and habitat use. This project will be the first application of a joint species-habitat cumulative effects assessment and represents a novel ecosystem-level approach to estimate cumulative impacts of human activities and climate change over large spatial scales. 

More specifically, the deliverables of the current project were the following:

1. Ecosystem-scale cumulative effects assessment combining species-scale and habitat-scale assessments.
   - Raw results of @halpern2008a and @beauchesne2023a methods
   - Geographical overlap between habitats and species distribution
   - Weightings for indirect effects to species arising from habitat use
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
  -  Multiplex figure of cumulative effects including:
     -  Subnetworks
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

<!-- Species interactions are a double-edged sword [@gilarranz2017]: those connections that are so vital to the existence of the complex ecological communities that inspired Darwin’s famous “*tangled bank*” metaphor are also those that allow the cumulative effects of climate change and human activities to spread through communities in a domino-like effect [@estes1974; @paine1980; @wootton2002; @bascompte2009a; @estes2011]. Stressors can rewire -- that is, reconfigure -- entire communities and alter energy flow in a system; for example, warming waters cause a northward advance of generalist fish species capable of disrupting both the structure and the strength of species interactions in the Arctic [@blanchard2015; @kortsch2015; @bartley2019]. Properly evaluating the effects of stressors on species should therefore consider species-specific sensitivities, the structure of local communities, and effects spreading through ecological interactions, *i.e.* trophically-mediated indirect effects [@beauchesne2021]. Management actions considering community structure and species interactions can lead to efficient and cost-effective outcomes, such as the restoration of kelp forests through the recovery of sea otters (*Enhydra lutris*), an important keystone species [@power1996] on the Pacific Coast of North America [@estes1974; @estes2010].

There is a growing demand for the management of the structural properties of whole communities to preserve ecosystems [@mccann2007; @tylianakis2010; @mcdonald-madden2016; @heinen2020] and for a broader application of regional cumulative effects assessments [@jones2016; @hodgson2019]. These are integral to an ecosystem-based approach to environmental management [@christensen1996]; still, species interactions remain conspicuously absent from the environmental management literature, biodiversity monitoring programs, the Intergovernmental Science-Policy Platform on Biodiversity and Ecosystem Services (IPBES) reports, and environmental regulations in general [@kollmann2016; @heinen2020]. Further, none of the headline or complementary indicators currently identified by the Parties to the Convention on Biological Diversity include the structure of ecological networks or trophic interactions [@cbd2023]. Despite the expanding wealth of literature on controlled multi-stressor and multi-species experiments [@orr2020], scaling these up to encompass entire ecosystems remains a significant challenge.

Here, we expand conventional cumulative effects assessment approaches (hereafter, species-scale assessment)[@maxwell2013; @halpern2019; @ohara2021] with recent progress in theoretical ecology [@stouffer2007; @stouffer2012; @beauchesne2021] to propose a novel method that captures the indirect propagation of the effects of stressors through species interactions (hereafter, network-scale assessment) and that is relevant to ecosystem-based management. We focus our network-scale assessment on the St. Lawrence marine ecosystem, in eastern Canada (see methods). This ecosystem is formed by one of the largest estuaries in the world and a vast interior sea. Together, they host diverse and productive ecological communities and provide a wealth of ecosystem services benefiting the Canadian economy: a rich commercial fisheries industry, a seaway that grants access to one of the most densely populated regions in North-America and more than 40 ports, an expanding aquaculture production, and a thriving tourism industry [@beauchesne2016; @schloss2017]. We demonstrate our approach by assessing and mapping the cumulative effects of 18 stressors on 193 species between 2010 and 2015. We use data-based or theoretically-derived indicators to characterize the distribution and intensity of stressors [@beauchesne2020], the distribution of species, the network of species interactions, and species-specific sensitivities to stressors; these are then combined through a theoretical framework [@beauchesne2021] to obtain a relative cumulative effect score for every species considered (see methods).  -->


<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->
# Materials and Methods
<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->

## Habitat-scale cumulative effects assessment model

The habitat-scale assessment was developped by @halpern2008a and requires three types of data modules: 1) the mapped presence or absence of habitats ($H_i$), 2) the spatial distribution and relative intensity of environmental stressors ($D_j$), and 3) the habitat-specific sensitivity of each habitat to each stressor ($\mu_{i,j}$). Data are incorporated into a grid made up of cells of homogeneous size characterizing the study area. Cumulative effect ($C_H$) are predicted for each grid cell ($x$) by summing all individual stressor effects over the set of habitats considered:

$$C_{H_x} = \sum_i \sum_j H_{i,x} * D_{j,x} * \mu_{i,j}$$

This method predicts relative cumulative effects that incorporates knowledge on habitat distribution, their exposure to environmental stressors, and their known or suspected sensitivity to the effects of stressors. The data modules used for the Scotian Shelf Bioregion habitat-scale cumulative effects assessment are described in @murphy2023. For this analysis, were subsampled the environmental stressors used in the assessment to match those used in the network-scale assessment of the Scotian Shelf Bioregion (see @beauchesne2023b). This results in an assessment of the effects of 17 stressors on 21 habitats in the Scotian Shelf Bioregion. 

## Network-scale cumulative effects assessment model

The approach developed by @beauchesne2023a builds on the method developped by @halpern2008a and combines it with recent progress in theoretical ecology [@stouffer2007; @stouffer2012; @beauchesne2021] to predict the net effects of environmental drivers by considering both direct and trophically-mediated indirect effects in ecological communities. By focusing on interactions rather than individual species, this approach provides the ability to consider how a focal species is affected by multiple environmental pressures, but also integrates how species it interacts with respond to the same pressures and how their response propagates to the species of interest.  

The method proposed by @beauchesne2023a decomposes food webs into eollections of $p$-species interactions called motifs [@milo2002]. Motifs that provide a mesoscale characterization of the structural properties of ecological networks [@bascompte2005; @stouffer2007; @stouffer2010; @stouffer2011; @bramonmora2018]. In a $n$-species food web ($n \geq p$), the collection of $p$-species motifs ($p \leq n$) in which species $k$ is involved in ($M_k = \{m_{k,1},m_{k,2},...,m_{k,x}\}$) forms its motif census ($M_k$) [@stouffer2012; @beauchesne2021]. The motif census provides an overview of all the interactions and connected species likely to affect a species’ dynamics, and the propagation of disturbances through their interactions. 

The network-scale model focuses on the most abundant 3-species motifs found in empirical food webs (*i.e.* trophic food chain, omnivory, exploitative and apparent competition) [@camacho2007; @stouffer2010] to assess a species motif census. Network-scale cumulative effects scores ($C_N$) are predicted in each grid cell $x$ as follows: 

$$C_{N_x} = \sum_k \frac{1}{|M_k|} \sum_{m_{k,x} \in M_k} \sum_j D_j * \overline{\mu_j} * T_{k_{m_{k,x}}}$$

where $k$ is the focal species, $M_k$ is the motif census of species $k$, $m_{k,x}$ are the 3-species motifs of interest forming species $k$'s motifs census, and $D_j$ is the log-transformed and scaled intensity of stressor $j$. 

$\overline{\mu_j}$ is the joint sensitivity of species in motif $m_{i,x}$ to stressor $j$. This joint sensitivity considers that a species' response to stressors integrates its own response as well as the response of species it interacts with. The joint sensitivity is measured as:

$$\overline{\mu_j} = w_1 \mu_{k,j} + w_2 \sum_l^2 \mu_{l,j}$$

where $\mu_{k,j}$ and $\mu_{l,j}$ are the sensitivities to stressor $j$ of focal species $k$ and of the two species interacting with species $k$ in motif $m_{k,x}$, respectively. 

$w_1$ and $w_2$ are weighting factors used to give a relative importance to direct -- *i.e.* effects to species $k$ -- and indirect -- *i.e.* effects propagating through species $l$ to species $k$ -- effects in the assessment. $w_1 + 2 w_2 = 1$ so that weighting provides a  percent contribution to direct and indirect effects. Here, we used $w1 = 0.5$ and $w2 = 0.25$ under the assumption that direct and indirect would have equal weights. 

$T_{k_{m_{k,x}}}$ is the trophic sensitivity of species $k$ in motif $m_{k,x}$. The trophic sensitivity of a species relates to a species propensity to be affected by trophically-mediated indirect effects. This sensitivity is related to the structure of the community, a species' trophic position, and the pathways of effects through which stressors are indirectly propagating to a species [@beauchesne2021]. 

In terms of data requirements, this network-scale assessment builds on the same 3 modules as @halpern2008a, *i.e.* 1) the [distribution of species](https://ecosystem-assessments.github.io/nceadfo/report/modules.html#spmodule) ($S_k$), 2) the [normalized distribution and intensity of environmental drivers](https://ecosystem-assessments.github.io/nceadfo/report/modules.html#drmodule) ($D_j$), and 3) the [species-specific sensitivity of each species to each driver](https://ecosystem-assessments.github.io/nceadfo/report/modules.html#sssmodule) ($\mu_{k,j}$); it considers 2 additional modules to complete the assessment: 4) the [metaweb of species interactions](https://ecosystem-assessments.github.io/nceadfo/report/modules.html#metamodule), *i.e.* the network of binary biotic interactions structuring local food webs, and 5) the [trophic sensitivity of species](https://ecosystem-assessments.github.io/nceadfo/report/modules.html#trsmodule) ($T_i$), *i.e.* their sensitivity to trophically-mediated indirect effects (see below for more details). These data modules are described in details in @beauchesne2023b for the Scotian Shelf species-scale assessment, and links in this text lead to relevant sections from that report. The data was used to perform an assessment of the effects of 17 stressors on 205 species of the Scotian Shelf Bioregion.

## Indirect effect to species through habitats

The network-scale assessment considers that indirect effects propagate from one species to another through their ecological interactions. Similarly, it is expected that indirect effects of stressors will propagate to species from effects to their habitats. Indirect effects to species arising from habitats were assessed as a function of cumulative effects to individual habitats and species habitat use in each grid cell $x$ as follows: 

$$I_{H_{k,x}} = \sum_i^{H_x} \frac{U_{i,x}}{\sum_{H_x} U_{k,x}} \sum_j D_{j,x} * \mu_{i,j}$$

where $k$ is the focal species, $i$ are habitats, $j$ are stressors, $H_x$ are habitats found in cell $x$, $D_j$ is the log-transformed and scaled intensity of stressor $j$, and $\mu_{i,j}$ is the habitat-specific sensitivity of each habitat to each stressor. Since multiple habitats may be found in a single cell, $U_{k_{i,x}}$ is the percent use of habitat $i$ by species $k$ in cell $x$, $\sum_{H_x} U_{k,x}$ is total percent habitat use by species $k$ in cell $x$, and dividing them together scales the contribution of each habitat to the indirect effects to species $k$ as a function of their total percent use in cell $x$. In our case, we considered that habitat were equally distributed within each $1 km^2$ grid cell and that species used the whole cell. The fraction for habitat used thus becomes $\frac{1}{|H_k|}$, where $|H_k|$ is the number of habitats used in cell $x$ by species $k$. 

Species and habitats are distributed in 3-dimension in marine environment, which is not captured by the 2-dimensional spatial data used for the assessment. This means, for example, that a pelagic species such as sea bird could be present in a grid cell where benthic habitats are also present without using the benthic habitats in actuality. To account for the 3-dimensional distribution of habitats and species, we used environmental traits to match which habitats may be used by which species. 

The environment in which species are found was extracted from the World Register of Marine Species (WoRMS) [@wormseditorialboard2017], FishBase [@froese2019], SeaLifeBase [@palomares2019], and the Encyclopedia of Life [@encyclopediaoflife2020] (see @beauchesne2023a for more details). Species were characterized either as bathydemersal (*i.e.* living and/or feeding on or near the bottom) and/or pelagic (*i.e.* occurring mainly in the water column) environments; species could span both types of habitats. 

Habitats, meanwhile, were characterized using the same categories. Habitats considered as spanning both bathydemersal and pelagic environments were the tidal zones (0-2m), nearshore habitats (2-30m), saltmarshes, seagrass beds, algal zones, kelp forests, and horse mussel bioherms. Habitats considered as only bathydemersal were shelf habitats (30-200m), bathyal habitats, canyons, and deep biogenic habitats. Habitat considered as only pelagic were pelagic habitats. 

We used this categorization for species and habitats to assess which habitats may be used by which species in each grid cell. Indirect effects predicted from habitats with a 3-dimensional mismatch in use were therefore set to zero in the assessment of the indirect effects to species arising through effects to habitats. 


## Joint cumulative effects to species 

Results from the network-scale assessment (*i.e.* direct stressors effects and indirect effects through species interactions) are then combined with the indirect effects to species through habitats. As the units from these assessments cannot be compared, we scale both assessments before combining them together. Results from the network-scale assessment were normalized between 0 and 2 to accound for the two types of effects considered in that assessment, while indirect effects to species through habitats was normalized between 0 and 1. Results from both assessment were then summed within each grid cell to provide a joint cumulative effects assessment to species that considers direct stressors effects and indirect effects arising through species interactions and habitats. 

## Ecosystem-scale cumulative effects assessment

Finally, the ecosystem-scale cumulative effects assessment combines results from the joint cumulative effects to species with the habitat-scale assessment in order to provide an overview of cumulative risk to species and habitats together, where each are considered as subnetworks. As with the joint assessment, units cannot be compared between both assessments. Both sets of results were therefore normalized between 0 and 1 before being combined into a single assessment. We also provide cumulative effects results scaled by the number of species (*i.e.* $n = 205$) or habitats (*i.e.* $n = 21$) within each subnetwork. 


## Assessment and spatial data representation

The open-source software R 4.2.3 was used for all analyses [@rcoreteam2023] and the package *rcea* [@beauchesne2023] was used to perform the cumulative effects assessment. See table S7 for a list of all R packages used. All datasets are presented at a 1 $km^2$ resolution even though some source data had coarser resolutions (table S1). We resampled and reprojected data when necessary using nearest neighbour estimates, which preserves the values of the source data. By doing so, we assume that the coarser data are evenly distributed across finer-scale cells with which they overlap. We used the NAD83 / Quebec Lambert projection (EPSG: 32198), which is well suited to represent and preserve surface area within our study system. 


<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->
# Results
<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->

## Network-scale and habitat-scale assessments 

\begin{figure}[H]
\centering
\includegraphics{./figures/ncea_ceahab.png}
\caption{Network-scale cumulative effects assessment of 17 stressors on 205 species in the Scotian Shelf Bioregion explicitly considering the underlying structure of the ecological community (\textbf{left-hand}). Habitats-scale cumulative effects assessment of 17 stressors on 21 marine habitats of the Scotian Shelf Bioregion (\textbf{right-hand}).}
\label{ncea_ceahab}
\end{figure}
\newpage


## Joint cumulative effects on species

\begin{figure}[H]
\centering
\includegraphics{./figures/nceahab_species.png}
\caption{}
\label{joint}
\end{figure}
\newpage


## Ecosystem-scale cumulative effects assessment

\begin{figure}[H]
\centering
\includegraphics{./figures/nceahab.png}
\caption{\textbf{À écrire}}
\label{ecosystem}
\end{figure}
\newpage

## Metaweb

\begin{figure}[H]
\centering
\includegraphics{./figures/metanetwork.png}
\caption{Multiplex network presenting the presence of an effect of individual stressors on each species and each habitat (colored links), indirect effects arising from ecological interactions (grey links), the indirect effects arising from habitat use (blue links), the indirect effects the overall cumulative effect on each species and habitats (species and habitat point sizes) and the mean effect of stressors (stressor point sizes).}
\label{metaweb}
\end{figure}
\newpage

## Contribution to cumulative effects

\begin{figure}[H]
\centering
\includegraphics{./figures/contribution_nceahab-2016_2021.png}
\caption{\textbf{À écrire}}
\label{contribution}
\end{figure}
\newpage




**Figure 3. Comparison of network-scale and species-scale cumulative effects for individual species.** Scatterplot of the mean network-scale cumulative effects as a function of the species-scale cumulative effect over a species distribution ($C / km^2$) for all 193 species considered. The size of the point is the degree of each species, *i.e* the number of interactions in which they are involved in the metaweb. The orange line represents the direct effects baseline, meaning that results from the network-scale assessment cannot fall below that line. Species along the orange line are those for which the network-scale and species-scale assessments are equal; that is species with no predicted interaction. The grey line is the 1:1 relative identity line (*i.e* from 0 to the maximum scores of the species-scale and network-scale assessments). Values above or below that line identify species for which the network-scale assessment is relatively greater or lower than the species-scale assessment, respectively. The yellow line represents the direct effects baseline, *i.e.* the minimum scores predicted when only direct effects are considered. Species with no predicted interactions in the metaweb follow that line. 

**Figure 4. Contribution of stressors and species to indirect effects in the network-scale cumulative effects assessment.** The upper half of the figure illustrates the mean contribution of climate (*n = 6*), coastal (*n = 5*), fisheries (*n = 5*), and marine traffic (*n = 2*) stressors to the regional cumulative effects assessment ($C / km^2$) on invertebrates grouped at the phyla taxonomic level (Arthropoda: *n = 30*; Cnidaria: *n = 20*; Echinodermata: *n = 21*; Mollusca: *n = 19*) and vertebrates grouped at the class taxonomic level (Actinopterygii: *n = 62*; Mammalia: *n = 24*). The contribution can be divided into the direct and indirect contributions of stressors to the cumulative effects of each taxonomic group. Direct effects are those attributable to the effect of stressors on a focal species, while indirect effects are the effects of stressors spreading through species interactions. The size of the point is proportional to the relative contribution of stressor groups to a taxonomic group. The grey gradient of boxes surrounding stressor points is proportional to the combined contribution of all stressors to direct or indirect effects, with low or high contributions presented as pale or dark grey, respectively. The lower half of the figure illustrates the mean contribution of taxonomic groups to the propagation of indirect effects to taxonomic groups. As with the stressors, the size of the points represents the relative contribution of taxonomic groups to indirect effects, and the grey gradient of boxes that of the combined contributions of invertebrates or vertebrates to indirect effects.




<!-- =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= -->
# References
<!-- =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= -->





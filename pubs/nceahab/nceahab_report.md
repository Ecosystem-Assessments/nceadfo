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
urlcolor: cyan
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

Note that all the code associated with this project is publicly available on GitHub at the following address: [https://github.com/inSilecoInc/nceahab](https://github.com/inSilecoInc/nceahab).

 
<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->
<!-- # Introduction -->
<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->



<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->
# Materials and Methods
<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->

Here, we are combining two cumulative effects assessments in which valued components are different, *i.e.* those components on which the assessments are performed. The assessments are performed at the scale of species [@beauchesne2023a] and habitats [@murphy2023], respectively. The cumulative effects predictions measured by both methods are in relative units rather than absolute units; they thus cannot be combined directly and need to be scaled in order to be comparable. The scaling assumes that the relative intensity of both assessments will be comparable, even if their units are not. This assumption is not new, as it is the basis of the cumulative effects prediction approach used by @murphy2023 and used as a foundation for @beauchesne2023a [see @halpern2008a]. The specific methods to assess cumulative effects are described in the following sections. Here, we briefly discuss how these assessments are combined together to provide an ecosystem-scale cumulative effects assessment (Figure 1). 

The network-scale assessment [@beauchesne2023a] provides an assessment of direct effects of stressors to species (1) and indirect effects of stressors arising from their propagation through species interactions (2). Effects to species predicted from the network-scale assessment are already combined and their comparability is discussed in the section presenting the approach. The habitat-scale assessment [@murphy2023] provides the means to predict two more sets of cumulative effects predictions. The first is the one directly used for an assessment of cumulative effects on habitats (3). The second combines effects to habitats with species habitat used to assess indirect effects of stressors to species arising from direct effects to their habitats (4). 

At the scale of species, results from the network-scale assessment (1, 2) and indirect effects to species arising from effects to their habitats (4) are combined into a joint cumulative effects to species (Figure 1). Results from the joint cumulative effects to species (1, 2, 4) are then combined to the habitat-scale assessment (3) to obtain the ecosystem-scale cumulative effects assessment (Figure 1). 

\begin{figure}[H]
\centering
\includegraphics{./figures/diagram.png}
\caption{\textbf{Diagram of assessment combination into an ecosystem-scale cumulative effects assessment.} The network-scale assessment can be divided into direct effects and indirect effects to species arising from species interaction. These results are combined with an assessment of indirect effects to species arising from direct effects to their habitats to measure joint cumulative effects to species. The ecosystem-scale cumulative effects assessment combines results from the habitat-scale assessment with the joint cumulative effects to species.}
\label{diagram}
\end{figure}
\newpage

## Habitat-scale cumulative effects assessment model

The habitat-scale assessment developed by @halpern2008a is a spatially explicit method that predicts relative cumulative effects on species for a set of stressors. Considering a study area divided into cells of homogeneous size, in a given cell $x$, the effect of a stressor $j$ on habitat $i$ is the product of three components: 1) the mapped presence or absence of habitat $i$, $H_{i,x}$; 2) the spatial distribution and relative intensity of environmental stressors $j$, $D_{j,x}$; 3) the habitat-specific sensitivity of habitat $i$ to stressor $j$ ($\mu_{i,j}$). The cumulative effect in $x$, $C_{H_x}$, is then computed as the sum of all products, i.e. the sum of the effect of all considered stressors on the entire set of habitats:

$$C_{H_x} = \sum_i \sum_j H_{i,x} D_{j,x} \mu_{i,j}$$

where $D_j$ is log-transformed and scaled. This method predicts relative cumulative effects that incorporates knowledge on habitat distribution, their exposure to environmental stressors, and their known or suspected sensitivity to the effects of stressors. The data modules used for the Scotian Shelf Bioregion habitat-scale cumulative effects assessment are described in @murphy2023. For this analysis, we subsampled the environmental stressors used in the assessment to match those used in the network-scale assessment of the Scotian Shelf Bioregion (see @beauchesne2023b). This results in an assessment of the effects of 17 stressors on 21 habitats in the Scotian Shelf Bioregion. See @beauchesne2023 for a [list of the stressors](https://ecosystem-assessments.github.io/nceadfo/report/modules.html#drmodule) used for this assessment.  


## Network-scale cumulative effects assessment model

The approach developed by @beauchesne2023a builds on the method developed by @halpern2008a and combines it with recent progress in theoretical ecology [@stouffer2007; @stouffer2012; @beauchesne2021] to predict the net effects of environmental stressors by considering both direct and trophically-mediated indirect effects in ecological communities. By focusing on interactions rather than individual species, this approach provides the ability to consider how a focal species is affected by multiple environmental pressures, but also integrates how species it interacts with will respond to the same pressures and how their response propagates to the species of interest.  

The method proposed by @beauchesne2023a decomposes food webs into collections of $p$-species interactions called motifs [@milo2002]. Motifs provide a mesoscale characterization of the structural properties of ecological networks [@bascompte2005; @stouffer2007; @stouffer2010; @stouffer2011; @bramonmora2018]. In a $n$-species food web ($n \geq p$), the collection of $p$-species motifs ($p \leq n$) in which species $k$ is involved ($M_k = \{m_{k,1},m_{k,2},...,m_{k,x}\}$) forms its motif census ($M_k$) [@stouffer2012; @beauchesne2021]. The motif census provides an overview of all the interactions and connected species likely to affect a species’ dynamics, and the propagation of disturbances through their interactions. 

The network-scale model focuses on the most abundant 3-species motifs found in empirical food webs (*i.e.* trophic food chain, omnivory, exploitative and apparent competition) [@camacho2007; @stouffer2010] to assess a species motif census. Network-scale cumulative effects scores ($C_N$) are predicted in each grid cell $x$ as follows: 

$$C_{N_x} = \sum_k \frac{1}{|M_k|} \sum_{m_{k,x} \in M_k} \sum_j D_j \overline{\mu_j} T_{k_{m_{k,x}}}$$

where $k$ is the focal species, $M_k$ is the motif census of species $k$, $m_{k,x}$ are the 3-species motifs of interest forming species $k$'s motifs census, and $D_j$ is the log-transformed and scaled intensity of stressors $j$. 

$\overline{\mu_j}$ is the joint sensitivity of species in motif $m_{i,x}$ to stressor $j$. This joint sensitivity considers that a species' response to stressors integrates its own response as well as the response of species it interacts with. The joint sensitivity is measured as:

$$\overline{\mu_j} = w_1 \mu_{k,j} + w_2 \sum_l^2 \mu_{l,j}$$

where $\mu_{k,j}$ and $\mu_{l,j}$ are the sensitivities to stressor $j$ of focal species $k$ and of the two species interacting with species $k$ in motif $m_{k,x}$, respectively. 

$w_1$ and $w_2$ are weighting factors used to give a relative importance to direct (*i.e.* effects to species $k$) and indirect (*i.e.* effects propagating through species $l$ to species $k$) effects in the assessment. $w_1 + 2 w_2 = 1$ so that weighting provides a percent contribution to direct and indirect effects. Here, we used $w1 = 0.5$ and $w2 = 0.25$ under the assumption that direct and indirect would have equal weights. 

$T_{k_{m_{k,x}}}$ is the trophic sensitivity of species $k$ in motif $m_{k,x}$. The trophic sensitivity of a species relates to a species propensity to be affected by trophically-mediated indirect effects. This sensitivity is related to the structure of the community, a species' trophic position, and the pathways of effects through which stressors are indirectly propagating to a species [@beauchesne2021]. 

In terms of data requirements, this network-scale assessment builds on the same 3 modules as @halpern2008a, *i.e.* 1) the [distribution of species](https://ecosystem-assessments.github.io/nceadfo/report/modules.html#spmodule) ($S_k$), 2) the [normalized distribution and intensity of environmental stressors](https://ecosystem-assessments.github.io/nceadfo/report/modules.html#drmodule) ($D_j$), and 3) the [species-specific sensitivity of each species to each driver](https://ecosystem-assessments.github.io/nceadfo/report/modules.html#sssmodule) ($\mu_{k,j}$); it considers 2 additional modules to complete the assessment: 4) the [metaweb of species interactions](https://ecosystem-assessments.github.io/nceadfo/report/modules.html#metamodule), *i.e.* the network of binary biotic interactions structuring local food webs, and 5) the [trophic sensitivity of species](https://ecosystem-assessments.github.io/nceadfo/report/modules.html#trsmodule) ($T_i$), *i.e.* their sensitivity to trophically-mediated indirect effects (see below for more details). For species, we assumed that phytoplankton and zooplankton species were present throughout the study area since these taxa are missing from our dataset and are required to properly consider trophic dynamics. These data modules are described in detail in @beauchesne2023b for the Scotian Shelf species-scale assessment, and links in this text lead to relevant sections from that report. The data was used to perform an assessment of the effects of [17 stressors](https://ecosystem-assessments.github.io/nceadfo/report/modules.html#drmodule) on [205 species](https://ecosystem-assessments.github.io/nceadfo/report/modules.html#spmodule) of the Scotian Shelf Bioregion. 

## Indirect effect to species through habitats

The network-scale assessment considers that indirect effects propagate from one species to another through their ecological interactions. Similarly, it is expected that indirect effects of stressors will propagate to species from effects to their habitats. Indirect effects to species arising from habitats were assessed as a function of cumulative effects to individual habitats and species habitat use in each grid cell $x$ as follows: 

$$I_{k,x} = \frac{1}{\sum_{i} H_{i,x} U_{k,i}} \sum_i H_{i,x} U_{k,i} \sum_j D_{j,x} \mu_{i,j}$$

where $k$ is the focal species, $i$ is a habitat, $j$ is a stressor, $H_{i,x}$ is the mapped presence or absence of habitat $i$, $D_j$ is the log-transformed and scaled intensity of stressor $j$, and $\mu_{i,j}$ is the habitat-specific sensitivity of each habitat to each stressor. $U_{k,i}$ is the affinity of species $k$ to habitat $i$ (more on this below). Since multiple habitats may be found in a single cell, we scale the effects predicted by $\sum{i} H_{i,x} U_{k,i}$ to account for the number of habitats used in a single cell. Note that $H_{i,x}$ could be an area for each habitat within each cell; we would then scale the predicted effects to the percent area used by each species in cell $x$. 

Species are not uniformly distributed among different habitat types and they may have greater or lower affinity for different types of habitats. Furthermore, species and habitats are distributed in 3-dimensions in the marine environment, which is not captured by the 2-dimensional spatial data used for the assessment. This means, for example, that a pelagic species such as sea birds could be present in a grid cell where benthic habitats are also present without using the benthic habitats in actuality. To account for the 3-dimensional distribution of habitats and species, we used environmental traits to match which habitats may be used by which species. 

The environment in which species are found was extracted from the World Register of Marine Species (WoRMS) [@wormseditorialboard2017], FishBase [@froese2019], SeaLifeBase [@palomares2019], and the Encyclopedia of Life [@encyclopediaoflife2020] (see @beauchesne2023a for more details). Species were characterized either as demersal (*i.e.* living and/or feeding on or near the bottom) and/or pelagic (*i.e.* occurring mainly in the water column) environments; species could span both types of habitats. 

Habitats, meanwhile, were characterized using the same categories. Habitats considered as spanning both demersal and pelagic environments were the tidal zones (0-2m), nearshore habitats (2-30m), saltmarshes, seagrass beds, algal zones, kelp forests, and horse mussel bioherms. Habitats considered as only demersal were shelf habitats (30-200m), bathyal habitats, canyons, and deep biogenic habitats. Habitat considered as only pelagic were pelagic habitats. 

We used this categorization for species and habitats to assess which habitats may be used by which species in each grid cell. Indirect effects predicted from habitats with a 3-dimensional mismatch in use were therefore set to zero in the assessment of the indirect effects to species arising through effects to habitats. 


## Joint cumulative effects to species 

Results from the network-scale assessment (*i.e.* direct stressors effects and indirect effects through species interactions) are then combined with the indirect effects to species through habitats. As the units from these assessments cannot be compared, we scale both assessments before combining them together. Results from the network-scale assessment were normalized between 0 and 2 to account for the two types of effects considered in that assessment, while indirect effects to species through habitats were normalized between 0 and 1. Results from both assessments were then summed within each grid cell to provide a joint cumulative effects assessment to species that considers direct stressors effects and indirect effects arising through species interactions and habitats. 

## Ecosystem-scale cumulative effects assessment

Finally, the ecosystem-scale cumulative effects assessment combines results from the joint cumulative effects to species with the habitat-scale assessment in order to provide an overview of cumulative risk to species and habitats together, where each is considered as a subnetwork. As with the joint assessment, units cannot be compared between both assessments. Both sets of results were therefore normalized between 0 and 1 before being combined into a single assessment. We also provide cumulative effects results scaled by the number of species (*i.e.* $n = 205$) or habitats (*i.e.* $n = 21$) within each subnetwork. 


## Assessment and spatial data representation

The open-source software R 4.2.3 was used for all analyses [@rcoreteam2023] and the package *rcea* [@beauchesne2023] was used to perform the cumulative effects assessment. All datasets are presented at a 1 $km^2$ resolution even though some source data had coarser resolutions. We resampled and reprojected data when necessary using nearest neighbour estimates, which preserves the values of the source data. By doing so, we assume that the coarser data are evenly distributed across finer-scale cells with which they overlap. We used the NAD83 / Quebec Lambert projection (EPSG: 32198), which is well suited to represent and preserve surface area within our study system. 


<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->
# Results
<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->

## Network-scale and habitat-scale assessments 

The assessment of cumulative effects to species (Figure 2A) reveals a contrast between the continental shelf, including the Bay of Fundy, and the continental slope. This is partly caused by the lower representation of [species](https://ecosystem-assessments.github.io/nceadfo/report/results.html#sprichres) and [environmental stressors](https://ecosystem-assessments.github.io/nceadfo/report/results.html#cumdrres) present on the continental slope. In particular, interpreting the relative absence of effects to the continental slope should take into account the absence of data characterizing environmental stressors in the study area. Still, ecological communities of the continental shelf are nonetheless more at risk from the cumulative effects of environmental stressors. 

On the continental shelf, the whole Bay of Fundy as well as southwestern Nova Scotia extending offshore towards the continental slope appears to be the region most at risk from cumulative effects. The regions northeast of Sable Island, as well as the coastal areas south of Cape Breton are also at greater risk from cumulative effects. Areas of greatest cumulative effects seem to be strongly associated with regions affected by a combination of climate-related stressors, commercial fisheries and shipping (see [Atlas](https://ecosystem-assessments.github.io/nceadfo/report/atlas.html) in @beauchesne2023b). Meanwhile, the middle of the continental shelf, besides a region associated with demersal destructive fisheries, as well as the northeastern corner of the study area close to the Laurentian Channel seem to be less at risk from cumulative effects. 


Result from the habitat-scale cumulative effects assessments are generally spatially consistent with the assessment of the network-scale assessment (Figure 2B). Predicted effects are greater on the continental shelf than on the continental slope, although effects to canyons of the continental slope are observed for this assessment. Also, effects to species in the Bay of Fundy appear to be relatively more intense than cumulative effects to habitats.

\begin{figure}[H]
\centering
\includegraphics{./figures/ncea_ceahab.png}
\caption{\textbf{Network-scale and habitat-scale cumulative effects assessments.} Network-scale cumulative effects assessment of 17 stressors on 205 species in the Scotian Shelf Bioregion in eastern Canada explicitly considering the underlying structure of the ecological community (\textbf{A}). Habitats-scale cumulative effects assessment of 17 stressors on 21 marine habitats of the Scotian Shelf Bioregion in eastern Canada (\textbf{B}).}
\label{ncea_ceahab}
\end{figure}
\newpage


## Joint cumulative effects on species

Unsurprisingly considering the agreement between cumulative effects at the species and habitat-scale (Figure 1), the combination direct (Figure 3B) and indirect effects (Figure 3C) to species with indirect effects through habitats (Figure 3D) generally provides a similar outlook on cumulative effects: greater effects in the Bay of Fundy and the continental shelf compared to the continental slope. However, indirect effects to species through effects to their habitats (Figure 3D) reveal some risks on the continental slope that were previously not visible. These effects seem to arise mainly from effects to pelagic habitats that are used by [sea birds](https://ecosystem-assessments.github.io/nceadfo/report/atlas.html#seabirds-1) on the continental slope.  


\begin{figure}[H]
\centering
\includegraphics{./figures/nceahab_species.png}
\caption{\textbf{Joint cumulative effects assessment.} Joint cumulative effects assessment of 17 stressors and on 205 species of the Scotian Shelf Bioregion in eastern Canada (\textbf{A}) taking into consideration direct effects of stressors (\textbf{B}), indirect effects propagating through species interactions (\textbf{C}), and indirect effects arising through cumulative effects to the habitats used by species (\textbf{D}).}
\label{joint}
\end{figure}
\newpage


## Ecosystem-scale cumulative effects assessment

The combination of the joint effects to species and the habitat-scale assessment provides the ecosystem-scale cumulative effects assessment (Figure 4). Since there are more species ($n = 205$) than habitats ($n = 21$), we provide an assessment that sums all individual assessments to species and habitats together (Figure 4A) and one that normalizes species-scale and habitat-scale assessments by the number of valued components that they each hold (Figure 4B). This is done to avoid masking effects to habitats due to the higher number of species considered. 

As before, this assessment nonetheless remains spatially consistent with previous results. On the continental shelf, the whole Bay of Fundy as well as southwestern Nova Scotia extending offshore towards the continental slope appears to be the region most at risk from cumulative effects. The regions northeast of Sable Island, as well as the coastal areas south of Cape Breton are also at greater risk from cumulative effects. Meanwhile, the middle of the continental shelf, besides a region associated with demersal destructive fisheries, as well as the northeastern corner of the study area close to the Laurentian Channel seem to be less at risk from cumulative effects. The continental slope, meanwhile, is characterized by lower cumulative effects, with the exception of indirect effects to seabirds through their habitat use and direct effects to canyons.

\begin{figure}[H]
\centering
\includegraphics{./figures/nceahab.png}
\caption{\textbf{Ecosystem-scale cumulative effects assessment.} Ecosystem-scale assessment of cumulative effects of 17 stressors on 21 marine habitats and 205 species of the Scotian Shelf Bioregion in eastern Canada using untransformed cumulative effects scores (\textbf{A}) and scores normalized by the number of habitats and species, respectively (\textbf{B}).}
\label{ecosystem}
\end{figure}
\newpage

## Metaweb

Figure 5 provides a visual representation of every individual pathways of effect (*i.e.* the links between stressors and their potential effects on various components of an ecosystem) through which stressors are predicted to affect species and habitats across the whole study area. These regional effects of environmental stressors on each species and habitat was assessed as the mean effect over the distribution of a species and a habitat, respectively. The greater the spatial distribution of a stressor, the greater its regional effect is likely to be; similarly, regional effects are likely to increase with a decrease of the distribution of a species or a habitat. In total, there are 6720 pathways of effects through which stressors are predicted to affect species ($n = 4596$) and habitats ($n = 214$). The greater number of pathways of effect to species is unsurprising considering that we are considering three types of effects for species (*i.e.* direct, indirect through species interactions and habitat use) compared to a single type of effect for habitats (*i.e.* direct). Results presented in figure 5 are not meant to be interpreted at the granular level. Rather, they are meant to provide an at-a-glance overview of all the individual predictions which the ecosystem-scale cumulative effects are built on and which would be explored in more detail. 


\begin{figure}[H]
\centering
\includegraphics{./figures/metanetwork.png}
\caption{Multiplex network presenting the presence of an effect of individual stressors on each species and each habitat (coloured links), indirect effects arising from ecological interactions (grey links), the indirect effects arising from species habitat use (blue links), the overall cumulative effect on each species and habitats (species and habitat point sizes) and the mean effect of stressors (stressor point sizes).}
\label{metaweb}
\end{figure}
\newpage

## Contribution to cumulative effects

Figure 6 complements Figure 5 by presenting the relative contribution of stressors to the overall effects predicted on species and habitats in a way that is easier to interpret and explore. Together, those figures offer interesting insights into the effects of environmental stressors on the species and habitats of the Scotian Shelf. The stressors with the most intense effects across both species and habitats are shipping, demersal destructive high-bycatch fisheries (*e.g.* trawls and dredges), and temperature anomalies; these are essentially some of the most widely distributed stressors that were considered in the assessment. At first glance, direct effects to habitats appears to be in general greater than those to species. However, but it must be reminded that predicted effects were scaled between 0 and 1 for both assessments. When comparing effects between habitats and species, both direct and indirect effects to species must therefore be considered in combination to allow a relative comparison between assessment. This means that the comparison must be made between direct effects to habitats and net effects (*i.e.* direct + indirect) to species. In that sense, relative effects to habitats and species appears generally similar in relative terms. 

In general, invertebrates appear more at risk from the cumulative effects of all stressors than vertebrates, especially from direct effects. This reflects broad taxa-specific sensitivities to the effects of environmental stressors.  The predicted risks from direct effects to invertebrates are in contrast with those for vertebrates, who appear to be at risk from the direct effects of few stressors, yet greatly at risk from all stressors when species interactions are taken into consideration to evaluate indirect effects. For instance, seabirds are generally unaffected by the direct effects of stressors; similarly, marine mammals and fish are almost exclusively affected by shipping and fisheries, respectively. However, almost all the seabirds, marine mammals and fish species considered are greatly affected by the indirect effects of all stressors. Unsurprisingly, indirect effects through habitat use predominantly originates from benthic and pelagic habitats for invertebrates and vertebrates, respectively. Both vertebrates and invertebrates are affected through nearshore habitats. Still, fish species are affected through a combination of all types of habitats. It must be noted that comparing the relative intensity of indirect effects arising from habitat use and ecological interactions is made difficult by the scaling necessary to combine the results. We must therefore be careful when interpreting indirect effects  through habitats with indirect effects through species interactions. 

Habitats, meanwhile, are predominantly affected by climate stressors. Benthic habitats are also at risk from demersal destructive fisheries in particular and shipping in some cases. Nearshore habitats, meanwhile, are affected by all stressor groups considered in the assessment. Canyons and deep pelagic habitats are the least affected habitats. 


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





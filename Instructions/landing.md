---
output: html_document
---
---------------------------------------------------------------------
<font color=#EE3A8C size=5>
This is a brief introduction of the CMTCN.
</font>

<font color=#104E8B size=4>
CMTCN is a freely available web tool that provides a comprehensive and intensive analysis of combinatorial regulatory interactions between transcription factors, microRNAs, and target genes.For each type of cancer, CMTCN identifies four types of 3-nodes TF-miRNA co-regulatory motifs (three FFLs and one co-regulation motif) consisting of a TF, a miRNA and their co-targeted gene. CMTCN can construct and depict cancer-specific co-regulatory networks in various tumor types, and meanwhile, it uncovers miRNA and TF co-regulatory relationships. On top of all that, CMTCN screens out important nodes in the co-regulatory network based on network topology analysis and provides genes/TFs/miRNAs enrichment according to different annotated datasets</font>
### I. Browse: Job submission

<font color=#00688B size=3>
CMTCN exhibits a neat and user-friendly layout. Users initiate their study by following three steps: 
</font>

#### (i) Choose a specific cancer
<font color=#00688B size=3>
CMTCN is available to show the study about 33 cancers.
</font>

![](1-Step1.png)

#### (ii) Select the knowledgebases to build the co-regulatory network

<font color=#00688B size=3>
The key step for constructing the TF-miRNA co-regulatory network is the identification of relationships among TFs, miRNAs and their joint targets. CMTCN utilizes different knowledgesbases which contain the information of the regulation of TF-Gene, TF-miRNA, and miRNA-gene/TF.Users are free to choose different regulatory databases to establish co-regulatory relationships.
</font>

![](1-Step2.png)

#### (iii) Construction of the co-regulatory network
<font color=#00688B size=3>
CMTCN supports full or partial network construction and analysis.One is to establish a complete co-regulatory network, and the other is to view the co-regulatory network for specific genes/miRNAs.
</font>

![](1-Step3.png)

#### a. Full co-regulatory network

<font color=#00688B size=3>
It will display the entire co-regulatory network of selected cancer.
</font>

#### b. Subgraph
<font color=#00688B size=3>
CMTCN enables the users to investigate the subgraph of the co-regulatory network which relates to specific genes/miRNAs. CMTCN pre-computed a number of related gene/miRNA gene sets.
</font>

##### Top mutated genes
<font color=#00688B size=3>
This data set involves top mutated genes for different types of cancer.
</font>

![](1-Step3-Mut.png)

##### Survival-related genes
<font color=#00688B size=3>
Significant genes related to survival times were identified in individual cancers and can be directly used as queries to map them to co-regulatory networks and evaluate their impact.
</font>

![](1-Step3-Sur.png)

##### Drug response-related genes
<font color=#00688B size=3>
This data set contains important genes related to drug response.
</font>

![](1-Step3-Drug.png)

##### Custom Input
<font color=#00688B size=3>
CMTCN supports multiple gene queries. Users can query their interested genes/miRNAs. The limit of the entities queried is 500.
</font>
  
![](1-Step3-Custom.png)  

##### Upload Local Data
<font color=#00688B size=3>
Users can also upload a local .txt file for the query. The text file requires one gene/miRNA per line, and the sample file can be downloaded.
</font>

![](1-Step3-Upload.png)

#### (iv) Click the submit button.
<font color=#00688B size=3>
The input data can be checked and modified through the input box.
After the input data being checked, just press the "submit" button to
submit the job and start the co-regulatory network research!
</font>

![](Step4_Submit.png)

### II. Analysis: The comprehensive analysis of cancer-specific co-regulatory network

#### (i) Network Visualization 
<font color=#00688B size=3>
The TF-miRNA co-regulatory network is constructed by merging the three types feed forward loops and significant TF-miRNA pairs together.
CMTCN pools regulatory relationships including TF-gene, TF-miRNA, miRNA-gene, miRNA-TF. Based on the regulatory relationships above, the significant co-regulatory pairs and 3-node feed-forward loops are identified. There are three types of the 3-node feed-forward loops: TF-FFLs, miRNA-FFLs and composite-FFLs. In a TF-FFL, TF is the master regulator, which regulates its partner miRNA and their joint target, while in a miRNA-FFL, miRNA is the master regulator. Together, TF-FFL and miRNA-FFL can combine into a composite-FFL where miRNA and TF regulate each other.
CMTCN adopts D3.js which depicts an interactive and intuitionistic force-directed graph for the co-regulatory network.
</font>

![](D3_1.png)

<font color=#00688B size=3>
Users can interactively modify the force-directed graph for better presentations by setting the link distance, node repulsion and the number of displayed co-regulatory relationships.
</font>

![](D3_2.png)

<font color=#00688B size=3>
In the cancer-specific co-regulatory network, there are yellow nodes, red nodes and green nodes representing genes, TFs and miRNAs respectively.

It should be mentioned that the default interactions displayed for each type of co-regulatory are limited to 100 for a better response and presentation.
</font>

<font color=#00688B size=3>
What is more, CMTCN has four color schemes for the network visualization. If you are fascinated about the network, save it!
</font>

![](D3_3.png)

#### (ii) Network Topology

<font color=#00688B size=3>
CMTCN provides detailed network topology infomation.
a.Network density. The proportion of present edges from all possible edges in the network.

![](Topology-1.png)

b.Nodes degree and degree distribution.

![](Topology-2.png)

c.Hubs and authorities.Hubs contains a large number of outgoing links; while authorities gets many incoming links from hubs.
</font>

![](Topology-3.png)

![](Topology-4.png) 

#### (iii) Co-regulatory Interactions

<font color=#00688B size=3>
CMTCN shows TFs/miRNAs/Genes information for each type of co-regulation. CMTCN incorporates TCGA expression data to refine the discoveries.
The P_TG,P_TM,P_MG represent expression Spearman correlation p-value between TF-Gene, TF-miRNA and miRNA-Gene respectively. The R_TG,R_TM,R_MG represent expression Spearman correlation coefficient between TF-Gene, TF-miRNA and miRNA-Gene respectively. CMTCN also shows the source regulatory knowledgebases which construct the co-regulatory relationships.
</font>

![](Co-Interactions.png)

#### (iv) Gene Enrichment

<font color=#00688B size=3>
CMTCN gives the gene enrichment information for genes/TFs in the network according to different annotated datasets. 
</font>

![](Gene-Enrichment-1.png)

![](Gene-Enrichment-2.png)



#### (v) miRNA Enrichment

<font color=#00688B size=3>
CMTCN gives the miRNA enrichment information for miRNAs in the network according to different annotated datasets. 
</font>

 ![](miRNA-Enrichment-1.png)
 
 ![](miRNA-Enrichment-2.png)

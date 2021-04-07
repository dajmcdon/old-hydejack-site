---
title: Stat 548 - Qualifying Papers
---

Last updated: 9 September 2020

## Choosing a paper

At the end of this document is a list of papers and project ideas that I am interested in supervising as Qualifying Papers (QPs). I am happy to discuss any other paper that you are interested in and think might be appropriate. I am generally interested in theoretical and methodological aspects of statistics and machine learning, especially those that relate to regularization, optimization, model selection, and time series forecasting.

## Expectations

If you are interested in doing a QP with me, the first step is to email me to schedule a one-on-one meeting. Please use the words “Qualifying Paper” in the subject line of your email. At our first meeting, please be prepared to discuss:
* Your background.
* Your long-term research interests (it’s okay if these are not yet well-defined).
* Why you are interested in the particular paper/project.
* When you will submit your report (typically about four-six weeks after we meet). 
* The details of the QP project and report.
* Any concerns you may have.

## Report

The report should have the following structure:[^1]

[^1]: Thanks to Trevor and Ben. I'm stealing most of this from them.

1. __Summary (~5 pages)__: The first section of the report should provide a summary of the paper and the problem(s) it addresses, including its relationship to any previous work, its major contributions (e.g., novel techniques, algorithmic developments, problem formulations, theoretical contributions), and any limitations or shortcomings (e.g., restrictive assumptions, computational constraints, flawed methodology). The aim of this section is for you to synthesize the findings of a body of work and clearly present the important points. 

2. __Mini-proposals for research projects__ Each proposal should describe a research project that applies, extends, generalizes, adapts, or addresses shortcomings of the QP. Seemingly unrelated ideas inspired by the original QP are also fine. You may write more than one proposal, but you must write at least one. A proposal should concisely describe: the primary problem to be addressed; an approach (or multiple approaches) for addressing the problem; any technical or conceptual sub-problems; the potential impact of the project. You are not expected to pursue any of these projects (though we can talk more if you would like to). The aim of this section is to get you thinking creatively about research, and to begin developing the skills necessary for writing research proposals. Each proposal should be no more than _2 pages max_.

3. __QP specific project results__ Each potential QP listed below has a brief description of a related project. We will discuss the project in detail in our initial meeting, and we can meet again (as many times as necessary) before the report due date. Your grade will not be affected by how good the results look, whether your approach improves on past work, or whether you achieve the initial goal of the project. I will use this project to evaluate your research potential, which includes (among other aspects):      
  - clearly formulating a research question;
  - setting up a useful mathematical framework for the problem;
  - thinking creatively and independently to develop a solution;
  - relating the problem to existing work, in other fields if necessary; 
  - being resourceful and asking questions when necessary;
  - learning from and moving past the inevitable setbacks;
  - reformulating the research problem when necessary;
  - implementing new methods in code (when applicable);
  - choosing appropriate experiments and metrics;
  - communicating and reflecting on progress, setbacks, and results; 
  - thinking of future research directions.
  
  The report should be submitted as a GitHub repository based on the template [here](https://github.com/dajmcdon/qp-template). The template includes a LATEX style file that should be used for the report. (Detailed instructions for usage can be found in the repository’s README file.) Any experimental/numerical results should be reproducible. All code should be reusable, clearly commented/documented, and exist in the `src/` folder of the same GitHub repository to which you give me access as a collaborator. Code can be in any language you wish, though my strong preference is for `R` or `python`.  


## Resources

Some resources on technical/mathematical writing:

* Nancy Heckman’s page on [writing](http://ugrad.stat.ubc.ca/~nancy/writing/)
* Harry Joe's [advice and writing resources for 548](https://www.stat.ubc.ca/~harry/papers/)
* Trevor Campbell’s [How to Explain Things talk](https://docs.google.com/presentation/d/13vwchlzQAZjjfiI3AiBC_kM-syI6GJKzbuZoLxgy1a4/edit#slide=id.g4fbcbb044c_0_0)
* Knuth, Larrabee, and Roberts on [mathematical writing](http://www.jmlr.org/reviewing-papers/knuth_mathematical_writing.pdf)
* Jenny Bryan's [Happy Git with R](https://happygitwithr.com)
* Getting started with Git: [chapters 1 and 2](https://git-scm.com/book/en/v2) should be all you need for this report


## Available papers

1. ~~Jahja, Farrow, Rosenfeld, Tibshirani. [Kalman Filter, Sensor Fusion, and Constrained Regression: Equivalences and Insights](https://papers.nips.cc/paper/9475-kalman-filter-sensor-fusion-and-constrained-regression-equivalences-and-insights)~~  
~~_Themes:_ Algorithms, time series, prediction~~  
~~_Project:_ any 1 of the 3 future work ideas described in the discussion section~~

2. ~~Johnson. [A Dynamic Programming Algorithm for the Fused Lasso and L0-Segmentation](https://doi.org/10.1080/10618600.2012.681238)~~  
~~_Themes:_ Algorithms, time series, trend filtering~~  
~~_Project:_ Implement Nick's algorithm for general losses. Compare it with the extended Kalman filter. Describe ways to use it for other estimators with different loss functions.~~

3. Deledalle. [Estimation of Kullback-Leibler losses for noisy recovery problems within the exponential family](https://projecteuclid.org/euclid.ejs/1503972028)  
_Themes:_ risk estimation, stat theory  
_Project:_ Compare and contrast methods of risk estimation for L1-regularized logistic regression. It's likely that reference [45] will be important, but ask me which parts.

4. ~~Suggala, Prasad, Ravikumar. [Connecting Optimization and Regularization Paths](https://papers.nips.cc/paper/8260-connecting-optimization-and-regularization-paths.pdf)~~  
~~_Themes:_ optimization, regularization, linear models~~  
~~_Project:_ Consider the simple case of ordinary least squares. How might one extend the results here for GD to Proximal GD? What can we say about the lasso path?~~

5. Schiavi, ..., Daducci. [A new method for accurate in vivo mapping of human brain connections using microstructural and anatomical information](https://advances.sciencemag.org/content/6/31/eaba8245.full)  
_Themes:_ neuroscience, optimization, regularization, linear models  
_Project:_ Replicate the results and extend them to more reasonable fibre models.

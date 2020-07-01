\name{freeassociationdata}
\docType{data}
\alias{freeassociationdata}
\title{Free association data to demonstrate wordgraph package functionality.}
\description{
Data that were collected in a research study on social representations. The participants were given a stimulus word regarding eight different life experiences (Diet, Exercise, Smoking, Alcoholic beverage, Nightlife, Lifestyle, Disease and Health) and asked to express three consecutively words associated with the stimulus word. Age, gender, marital status (married) and educational level (secondary) was also recorded. The nine Symptom Check List (SCL - 90R) psychopathology subscales are also administered and included in the data. Permission to make data publicly available was granted from Professor Mary Gouva.
}
\usage{freeassociationdata}
\format{
  A data frame with 294 observations on the following 41 variables.
  \describe{
    \item{\code{aa}}{Index}
    \item{\code{age}}{Age}
    \item{\code{gender}}{Gender}
    \item{\code{married}}{Marital status}
    \item{\code{educationallevel}}{Secondary education}
    \item{\code{diet1stword}}{First word (translated in English) that were associated to the stimulus word "Diet"}
    \item{\code{diet2ndword}}{Second word (translated in English) that were associated to the stimulus word "Diet"}
    \item{\code{diet3rdword}}{Third word (translated in English) that were associated to the stimulus word "Diet"}
    \item{\code{exercise1stword}}{First word (translated in English) that were associated to the stimulus word "Exercise"}
    \item{\code{exercise2ndword}}{Second word (translated in English) that were associated to the stimulus word "Exercise"}
    \item{\code{exercise3rdword}}{Third word (translated in English) that were associated to the stimulus word "Exercise"}
    \item{\code{smoking1stword}}{First word (translated in English) that were associated to the stimulus word "Smoking"}
    \item{\code{smoking2ndword}}{Second word (translated in English) that were associated to the stimulus word "Smoking"}
    \item{\code{smoking3rdword}}{Third word (translated in English) that were associated to the stimulus word "Smoking"}
    \item{\code{alcohol1stword}}{First word (translated in English) that were associated to the stimulus word "Alcohol consumption"}
    \item{\code{alcohol2ndword}}{Second word (translated in English) that were associated to the stimulus word "Alcohol consumption"}
    \item{\code{alcohol3rdword}}{Third word (translated in English) that were associated to the stimulus word "Alcohol consumption"}
    \item{\code{nightlife1stword}}{First word (translated in English) that were associated to the stimulus word "Nightlife"}
    \item{\code{nightlife2ndword}}{Second word (translated in English) that were associated to the stimulus word "Nightlife"}
    \item{\code{nightlife3rdword}}{Third word (translated in English) that were associated to the stimulus word "Nightlife"}
    \item{\code{lifestyle1stword}}{First word (translated in English) that were associated to the stimulus word "Lifestyle"}
    \item{\code{lifestyle2ndword}}{Second word (translated in English) that were associated to the stimulus word "Lifestyle"}
    \item{\code{lifestyle3rdword}}{Third word (translated in English) that were associated to the stimulus word "Lifestyle"}
    \item{\code{disease1stword}}{First word (translated in English) that were associated to the stimulus word "Disease"}
    \item{\code{disease2ndword}}{Second word (translated in English) that were associated to the stimulus word "Disease"}
    \item{\code{disease3rdword}}{Third word (translated in English) that were associated to the stimulus word "Disease"}
    \item{\code{health1stword}}{First word (translated in English) that were associated to the stimulus word "Health"}
    \item{\code{health2ndword}}{Second word (translated in English) that were associated to the stimulus word "Health"}
    \item{\code{health3rdword}}{Third word (translated in English) that were associated to the stimulus word "Health"}
    \item{\code{diet1stword_el}}{First word (original Greek) that were associated to the stimulus word "Diet"}
    \item{\code{diet2ndword_el}}{Second word (original Greek) that were associated to the stimulus word "Diet"}
    \item{\code{diet3rdword_el}}{Third word (original Greek) that were associated to the stimulus word "Diet"}
    \item{\code{exercise1stword_el}}{First word (original Greek) that were associated to the stimulus word "Exercise"}
    \item{\code{exercise2ndword_el}}{Second word (original Greek) that were associated to the stimulus word "Exercise"}
    \item{\code{exercise3rdword_el}}{Third word (original Greek) that were associated to the stimulus word "Exercise"}
    \item{\code{smoking1stword_el}}{First word (original Greek) that were associated to the stimulus word "Smoking"}
    \item{\code{smoking2ndword_el}}{Second word (original Greek) that were associated to the stimulus word "Smoking"}
    \item{\code{smoking3rdword_el}}{Third word (original Greek) that were associated to the stimulus word "Smoking"}
    \item{\code{alcohol1stword_el}}{First word (original Greek) that were associated to the stimulus word "Alcohol consumption"}
    \item{\code{alcohol2ndword_el}}{Second word (original Greek) that were associated to the stimulus word "Alcohol consumption"}
    \item{\code{alcohol3rdword_el}}{Third word (original Greek) that were associated to the stimulus word "Alcohol consumption"}
    \item{\code{nightlife1stword_el}}{First word (original Greek) that were associated to the stimulus word "Nightlife"}
    \item{\code{nightlife2ndword_el}}{Second word (original Greek) that were associated to the stimulus word "Nightlife"}
    \item{\code{nightlife3rdword_el}}{Third word (original Greek) that were associated to the stimulus word "Nightlife"}
    \item{\code{lifestyle1stword_el}}{First word (original Greek) that were associated to the stimulus word "Lifestyle"}
    \item{\code{lifestyle2ndword_el}}{Second word (original Greek) that were associated to the stimulus word "Lifestyle"}
    \item{\code{lifestyle3rdword_el}}{Third word (original Greek) that were associated to the stimulus word "Lifestyle"}
    \item{\code{disease1stword_el}}{First word (original Greek) that were associated to the stimulus word "Disease"}
    \item{\code{disease2ndword_el}}{Second word (original Greek) that were associated to the stimulus word "Disease"}
    \item{\code{disease3rdword_el}}{Third word (original Greek) that were associated to the stimulus word "Disease"}
    \item{\code{health1stword_el}}{First word (original Greek) that were associated to the stimulus word "Health"}
    \item{\code{health2ndword_el}}{Second word (original Greek) that were associated to the stimulus word "Health"}
    \item{\code{health3rdword_el}}{Third word (original Greek) that were associated to the stimulus word "Health"}
    \item{\code{SOM}}{Somatization}
    \item{\code{OC}}{Obsessive–Compulsive }
    \item{\code{IS}}{Interpersonal Sensitivity }
    \item{\code{DEP}}{Depression}
    \item{\code{ANX}}{Anxiety}
    \item{\code{HOS}}{Hostility }
    \item{\code{PHB}}{Phobia}
    \item{\code{PAR}}{Paranoid Ideation }
    \item{\code{PSY}}{Psychoticism}
    \item{\code{GSI}}{Global Severity Index}
    \item{\code{OAS}}{Other As Shamer Scale score}
    \item{\code{ESS}}{Experiences of Shame Scale score  }

  }
}
\source{
The Social Representations of Health as a common Symbolic Place. E. Briseniou, N Skenteris, C. Hatzoglou, E. Diamantopoulos & M. Gouva. (Article 1).

Psychopathology and shame effect on words selected to describe eight common social representations.
E. Briseniou, N Skenteris, C. Hatzoglou, E. Diamantopoulos & M. Gouva. (Article 2).
}
\keyword{datasets}

#' CSIbank
#' 
#' 
#' 
#' @docType data
#' @name csibank
#' @usage csibank
#' @format A data frame with 1707 observations on the following 32 variables. The first five variables 
#' are segmentation variables. The rest of the variables refer to seven latent concepts: 1) 
#' \code{IMAG}=Image, 2) \code{EXPE}=Expectations, 3) \code{QUAL}=Quality, 4) \code{VAL}=Value, 
#' 5) \code{SAT}=Satisfaction, and 6) \code{LOY}=Loyalty. 
#'  Variables description
#' \itemize{
#'      \item{\code{IMAG}}: Includes variables such as reputation, trustworthiness, seriousness, solidness, and caring about customer's needs. \cr
#'      \item{\code{EXPE}}: Includes variables such as products and services provided, customer service, providing solutions, and expectations for the overall quality. \cr
#'      \item{\code{QUAL}}: Includes variables such as reliable products and services, range of products and services, personal advice, and overall perceived quality. \cr
#'      \item{\code{VAL}}: Includes variables such as beneficial services and products, valuable investments, quality relative to price, and price relative to quality. \cr
#'      \item{\code{SAT}}: Includes variables such as overall rating of satisfaction, fulfillment of expectations, satisfaction relative to other banks, and performance relative to customer's ideal bank. \cr
#'      \item{\code{LOY}}: Includes variables such as propensity to choose the same bank again, propensity to switch to other bank, intention to recommend the bank to friends, and sense of loyalty. \cr
#' }
#'
#' Manifest variables description
#'
#'\itemize{
#'\item{\code{imag1}}{First MV of the block Image}
#'\item{\code{imag2}}{Second MV of the block Image}
#'\item{\code{imag3}}{Third MV of the block Image}
#'\item{\code{imag4}}{Fourth MV of the block Image}
#'\item{\code{imag5}}{Fifth MV of the block Image}
#'\item{\code{imag6}}{Sixth MV of the block Image}
#'\item{\code{expe1}}{First MV of the block Expectations}
#'\item{\code{expe2}}{Second MV of the block Expectations}
#'\item{\code{expe3}}{Third MV of the block Expectations}
#'\item{\code{expe4}}{Fourth MV of the block Expectations}
#'\item{\code{qual1}}{First MV of the block Quality}
#'\item{\code{qual2}}{Second MV of the block Quality}
#'\item{\code{qual3}}{Third MV of the block Quality}
#'\item{\code{qual4}}{Fourth MV of the block Quality}
#'\item{\code{qual5}}{Fifth MV of the block Quality}
#'\item{\code{qual6}}{Sixth MV of the block Quality}
#'\item{\code{qual7}}{Seventh MV of the block Quality}
#'\item{\code{val1}}{First MV of the block Value}
#'\item{\code{val2}}{Second MV of the block Value}
#'\item{\code{val3}}{Third MV of the block Value}
#'\item{\code{val4}}{Fourth MV of the block Value}
#'\item{\code{sat1}}{First MV of the block Satisfaction}
#'\item{\code{sat2}}{Second MV of the block Satisfaction}
#'\item{\code{sat3}}{Third MV of the block Satisfaction}
#'\item{\code{loy1}}{First MV of the block Loyalty}
#'\item{\code{loy2}}{Second MV of the block Loyalty}
#'\item{\code{loy3}}{Third MV of the block Loyalty}
#'}
#' 
#' Segmentation Variables description 
#'\itemize{
#'\item{\code{Gender}}{a factor with levels \code{Female} \code{Male}}
#'\item{\code{Age}}{a factor with levels \code{<=25} \code{>=66} \code{26-35} \code{36-45} \code{46-55} \code{56-65}}
#'\item{\code{Education}}{a factor with levels \code{Elementary} \code{Graduated} \code{Highschool} \code{Undergrad} \code{Unfinished}}
#'\item{\code{Occupation}}{a factor with levels \code{Manager} \code{MediumEmplo} \code{Notemploy} \code{OwnFreelan} \code{Retired}}
#'\item{\code{Region}}{a factor with levels \code{Center} \code{East} \code{North}}
#'}
#'
#'@references Lamberti, G. (2014) \emph{Modeling with Heterogeneity.} PhD Dissertation.
#' @source Laboratory of Information Analysis and Modeling (LIAM). 
#'    Facultat de Informatica de Barcelona, Universitat Politecnica de Catalunya.
#' @keywords datasets
NULL


#' Fibtelereg
#' 
#' Fibtelereg dataset 
#' 
#' @docType data
#' @name fibtelereg
#' @usage fibtelereg
#' @format A data frame with 147 observations on the following 18 variables. The first ten variables 
#' are segmentation variables. The rest of the variables refer to five variables 1) 
#' \code{Image} = Image, 2) \code{Exp.spec} = Specific Expectation, 3) \code{Exp.gen} = Generic Expectation,
#' 4)\code{Qual.spec} = Specific Quality, 5) \code{Qual.gen} = Generic Quality, 6) \code{Value} = Value, 7) 
#' \code{Satis} = Satisfaction. 
#'  Variables description
#' \itemize{
#' \item{\code{Image}}: Generic students perception of ICT schools: (internationally recognized, 
#'      ranges of courses, leader in research).
#' \item{\code{Exp.spec}}: Specific Expectation on specific skills (technic or applied skills).
#' \item{\code{Exp.gen}}: Generic Expectation on generic skills (abilities in problem solving, 
#'  communication skills).
#' \item{\code{Qual.spec}}: Perception about the achieved quality on the specific skills in the school.
#' \item{\code{Qual.gen}}: Perception about achieved quality on the  generic skills in 
#' the school (abilities in solving problem, communication skills).
#' \item{\code{Value}}: The advantage or profit that the alumni may draw from the school 
#' degree (well paid job, motivated job, prospectives in improvement and promotion).
#' \item{\code{Satis}}: Degree of alumni satisfaction about the formation in school respect to 
#' their actual work conditions.
#' }
#' 
#' Segmentation Variables description 
#'\itemize{
#'\item{\code{Career}}{a factor with levels \code{EI} \code{ETS} \code{TEL}}
#'\item{\code{Gender}}{a factor with levels \code{female} \code{male}}      
#'\item{\code{Age}}{a factor with levels \code{25-26years} \code{27-28years} \code{29-30years} \code{31years+}}   
#'\item{\code{Studying}}{a factor with levels \code{no.stud} \code{yes.stud}}    
#'\item{\code{Contract}} {a factor with levels \code{fix.cont} \code{other.cont} \code{temp.cont}} 
#'\item{\code{Salary}}{a factor with levels \code{18k} \code{>45k} \code{25k} \code{35k} \code{45k}}  
#'\item{\code{Firmtype}}{a factor with levels \code{priva} \code{publi}}       
#'\item{\code{Accgrade}}{a factor with levels \code{7-8accnote} \code{ accnote<7} \code{accnote>8}}      
#'\item{\code{Grade}}{a factor with levels \code{<6.5note} \code{>7.5note} \code{6.5-7note} \code{7-7.5note}}     
#'\item{\code{Startwork}}{a factor with levels \code{after.grad} \code{befor.grad}}
#'}
#' @references Lamberti, G. (2014) \emph{Modeling with Heterogeneity.} PhD Dissertation.
#' @source Laboratory of Information Analysis and Modeling (LIAM). 
#'    Facultat de Informatica de Barcelona, Universitat Politecnica de Catalunya.
#' @keywords datasets
NULL

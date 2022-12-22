#' csibank
#' 
#' @docType data
#' @name csibank
#' @usage csibank
#' @format csibank data refers to a 2008 marketing research study of a
#' leading Spanish firm providing retail financial services. For confidentiality reasons, 
#' the complete details of the survey-based study will not be provided, only a description 
#' of the variables. The data include a total of 32 variables, measured for
#' 1707 clients. The 32 variables are divided into two groups: a group 
#' formed of 27 indicator variables for the structural model, and a group 
#' formed of 5 categorical variables. Each block of indicators refers to a specific construct.
#' 
#' Latent variable description:
#' \itemize{
#'      \item{\code{IMAG}}: Includes variables such as reputation, trustworthiness, seriousness, solidness, and caring about customer's needs \cr
#'      \item{\code{EXPE}}: Includes variables such as products and services provided, customer service, solution provision, and expectations for overall quality \cr
#'      \item{\code{QUAL}}: Includes variables such as reliable products and services, range of products and services, personal advice, and overall perceived quality \cr
#'      \item{\code{VAL}}: Includes variables such as beneficial services and products, valuable investments, quality relative to price, and price relative to quality \cr
#'      \item{\code{SAT}}: Includes variables such as overall satisfaction rating, fulfillment of expectations, satisfaction relative to other banks, and performance relative to an ideal bank \cr
#'      \item{\code{LOY}}: Includes variables such as likelihood of choosing the same bank again,  likelihood of switching to another bank, intention to recommend the bank to friends, and feeling of loyalty \cr
#' }
#'
#' Manifest variables description:
#'\itemize{
#'\item{\code{imag1}: }{Bank's reputation}
#'\item{\code{imag2}: }{Trustworthiness}
#'\item{\code{imag3}: }{Bank's solidity}
#'\item{\code{imag4}: }{Innovation and forward looking}
#'\item{\code{imag5}: }{Bank's emphasis on public affairs}
#'\item{\code{imag6}: }{Caring about the customer’s needs}
#'\item{\code{expe1}: }{Providing products and services to meet the customer's needs}
#'\item{\code{expe2}: }{Providing customer service}
#'\item{\code{expe3}: }{Providing solutions to daily banking needs}
#'\item{\code{expe4}: }{Expectations of overall quality}
#'\item{\code{qual1}: }{Reliable products and services}
#'\item{\code{qual2}: }{Range of products and services}
#'\item{\code{qual3}: }{Degree to which customer feels well informed}
#'\item{\code{qual4}: }{Personal advice}
#'\item{\code{qual5}: }{Customer service}
#'\item{\code{qual6}: }{Overall rating of perceived quality}
#'\item{\code{qual7}: }{Overall rating of satisfaction}
#'\item{\code{val1}: }{Beneficial services and products}
#'\item{\code{val2}: }{Valuable investments}
#'\item{\code{val3}: }{Quality relative to price}
#'\item{\code{val4}: }{Price relative to quality}
#'\item{\code{sat1}: }{Overall rating of satisfaction}
#'\item{\code{sat2}: }{Fulfillment of expectations}
#'\item{\code{sat3}: }{Rating the performance relative to customer's ideal bank}
#'\item{\code{loy1}: }{Propensity to choose the same bank if the customer had to choose again}
#'\item{\code{loy2}: }{Propensity to switch to other banks if they offered better terms}
#'\item{\code{loy3}: }{Customer's intention to recommend the bank to friends or colleagues}
#'}
#' 
#' Categorical variables description: 
#'\itemize{
#'\item{\code{Gender}: }{Gender of the customers, a factor with levels: \code{Female} and \code{Male}}
#'\item{\code{Age}: }{Age of the customers, a factor with levels: \code{<=25}, \code{26-35}, \code{36-45}, \code{46-55}, \code{56-65}, and \code{>=66}}
#'\item{\code{Education}: }{Education of the customers, a factor with levels: \code{Elementary}, \code{Graduated}, \code{Highschool}, \code{Undergrad}, and \code{Unfinished}}
#'\item{\code{Occupation}: }{Occupation of the customers, a factor with levels: \code{Manager}, \code{MediumEmplo}, \code{Notemploy}, \code{OwnFreelan}, and \code{Retired}}
#'\item{\code{Region}: }{Region of residence of the customers, a factor with levels: \code{Center}, \code{East}, \code{North}}
#'}
#'
#'
#'
#'@references Lamberti, G. et al. (2017). The Pathmox approach for PLS path modeling: discovering 
#' which constructs differentiate segments.\emph{Applied Stochastic Models in Business and Industry}, 
#' doi: 10.1002/asmb.2270.
#' 
#'@references Lamberti, G. (2014) \emph{Modeling with Heterogeneity.} PhD Dissertation.
#'
#' @source Laboratory of Information Analysis and Modeling (LIAM). 
#'    Facultat de Informatica de Barcelona, Universitat Politecnica de Catalunya.
#' @keywords datasets
NULL
#'
#' footdata
#' 
#' @docType data
#' @name footdata
#' @usage footdata
#' @format footdata data refers to a marketing research study from 2018 concerning
#' the experience of watching a football match in the stadium of a top Spanish football club. 
#' For confidentiality reasons, 
#' the complete details of the survey-based study will not be provided. Only the description 
#' of the variables is given. The data consist of a total of 20 variables, measured for
#' 362 spectators. The 20 variables are divided into two groups. One group is 
#' formed of 15 indicator variables for the structural model, and the other group 
#' is formed of 5 categorical variables. Each block of indicator refers to a specific construct.
#' 
#' 
#' Latent variable description
#' \itemize{
#'      \item{\code{QUA}}: Spectators’ perception of service performance, based on evaluations of service dimensions \cr
#'      \item{\code{IMA}}: Spectators’ perception of the attributes, players, management and condition of the club \cr
#'      \item{\code{SAT}}: Spectators’ evaluation of the perceived benefits of attending a match \cr
#'      \item{\code{LOY}}: Spectators’ deeply held commitment to repeat and recommend assisting a match \cr
##' }
#'
#' Manifest variables description
#'\itemize{
#'\item{\code{QUA1}: }{Tickets (availability, information, and staff attitudes)}
#'\item{\code{QUA2}: }{Accessibility (accessibility, signposting, queue safety, and admission organization)}
#'\item{\code{QUA3}: }{Facilities (shops, restrooms, seating, restaurants)}
#'\item{\code{QUA4}: }{Stadium (loudspeakers, appearance, staff competence, cleanliness, Security/safety)}
#'\item{\code{IMA1}: }{It is a great club}
#'\item{\code{IMA2}: }{It has a good team}
#'\item{\code{IMA3}: }{It is an ambitious club}
#'\item{\code{IMA4}: }{It is a heroic club}
#'\item{\code{SAT1}: }{The stadium experience was satisfactory}
#'\item{\code{SAT2}: }{The time spent at the stadium was worth it}
#'\item{\code{SAT3}: }{The money spent at the stadium was worth it}
#'\item{\code{SAT4}: }{The experience met all my expectations}
#'\item{\code{SAT5}: }{Overall satisfaction}
#'\item{\code{LOY1}: }{I would recommend the experience at this stadium}
#'\item{\code{LOY2}: }{I intend to repeat the experience at this stadium}
#'}
#' 
#' Categorical Variables description 
#'\itemize{
#'\item{\code{gender}: }{Gender of the spectators, a factor with levels: \code{FEMALE} and \code{MALE}}
#'\item{\code{age}: }{Age of the spectators, a factor with levels \code{<=30}, \code{31-45}, and \code{>=46}}
#'\item{\code{tourist}: }{Whether the spectator was a tourist or not, a factor with levels: \code{YES} or \code{NO}}
#'\item{\code{companion}: }{Who Accompanying the spectator, a factor with levels: \code{FRIENDS}, \code{FAMILY}, and \code{OTHERS}}
#'\item{\code{Involvement}: }{Degree of involvement, a factor with levels: \code{FAN}, \code{MEMBER}, and \code{OTHERS}}
#'}
#'
#'
#'
#'@references Lamberti, G., Rialp, J., and Simon, A. (2021). Antecedents of satisfaction and 
#'loyalty in different spectator tribes in a football context. 
#'\emph{International Journal of Sports Marketing and Sponsorship},
#' doi: 10.1108/IJSMS-12-2020-0210; 
#' 
#' @source Universitat Autonoma of Barcelona . 
#'    Business departament, Universitat Autonoma de Barcelona.
#' @keywords datasets
NULL
#' 
#' climate
#' 
#' @docType data
#' @name climate
#' @usage climate
#' @format climate data refers to the younger emplyees (<=30) of an international bank organizational study. It 
#' consists of 32 variables for 669 employees divided into 3 qualitative variables and 29 indicator (or manifest) 
#' variables. The 3 categorical variables, reflecting specific employee characteristics  
#' as potential observed sources of heterogeneity, gender, job level, 
#' and seniority. The 31 indicator (or manifest) 
#' variables were loyalty (3 indicators), satisfaction (6 indicators), 
#' and the 5 work climate constructs of empowerment (5 indicators), 
#' company reputation (3 indicators), pay (4 indicators), work conditions (3 indicators), 
#' leadership (5 indicators)
#' 
#' 
#' Latent variable description
#' \itemize{
#'      \item{\code{Loyalty}}: Employee commitment to the organization \cr  
#'      \item{\code{Satisfaction}}: Employee contentedness with their job and aspects such as kind of work, supervision, etc \cr
#'      \item{\code{Empowerment}}: Employee perceptions of management vision (vertical or horizontal) \cr
#'      \item{\code{Reputation}}: Employee perceptions of the company’s reputation \cr
#'      \item{\code{Leadership}}: Employee perceptions of leadership practices such as feedback and appraisal \cr
#'      \item{\code{Pay}}: Employee perceptions of pay and of its equity \cr
#'      \item{\code{Conditions}}: Employee perceptions regarding the conditions in which they perform their work \cr
#'      
##' }
#'
#' Manifest variables description
#'\itemize{
#'\item{\code{Emp1}: }{Employee treatment as responsible}
#'\item{\code{Emp2}: }{Teamwork is empowered}
#'\item{\code{Emp3}: }{Autonomy is favored}
#'\item{\code{Emp4}: }{Confidence in performed tasks}
#'\item{\code{Emp5}: }{Creativity and initiative are endorsed}
#'\item{\code{Imag1}: }{Organization’s reputation}
#'\item{\code{Imag2}: }{Organization’s values}
#'\item{\code{Imag3}: }{Organization’s customer relationships}
#'\item{\code{Pay1}: }{Salary}
#'\item{\code{Pay2}: }{Social benefits}
#'\item{\code{Pay3}: }{My salary corresponds to my duties}
#'\item{\code{Pay4}: }{My salary corresponds to my effort}
#'\item{\code{Work1}: }{Enough personnel in the office}
#'\item{\code{Work2}: }{Enough time to perform the tasks}
#'\item{\code{Work3}: }{Conditions and tools for work}
#'\item{\code{Lead1}: }{Agenda and planning}
#'\item{\code{Lead2}: }{Receptiveness}
#'\item{\code{Lead3}: }{Encouraging}
#'\item{\code{Lead4}: }{Communication}
#'\item{\code{Lead5}: }{Celebrating success}
#'\item{\code{Sat1}: }{Overall rating of satisfaction}
#'\item{\code{Sat2}: }{Tasks in accordance with capabilities}
#'\item{\code{Sat3}: }{Possibility to know efficiency}
#'\item{\code{Sat4}: }{Possibility to learn new things}
#'\item{\code{Sat5}: }{Usefulness of performed job}
#'\item{\code{Sat6}: }{Fulfilment of expectations}
#'\item{\code{Loy1}: }{I am unwilling to leave in case of not finding alternative}
#'\item{\code{Loy2}: }{I am committed to the institution}
#'\item{\code{Loy3}: }{I trust in the proper direction of the management}
#'}
#' 
#' Segmentation variables description 
#'\itemize{
#'\item{\code{Gender}: }{Gender of the employees, a factor with levels: \code{male} and \code{female}}
#'\item{\code{Level}: }{Level of the employees, a factor with levels: \code{low}, \code{medium}, and \code{high}}
#'\item{\code{Seniority}: }{Time working in the bank, a factor with levels: \code{<5y} and \code{15-5y}}
#'}
#'
#'
#'
#'@references Lamberti, G., Aluja Banet, T., & Rialp Criado, J. (2020). 
#'Work climate drivers and employee heterogeneity. 
#'\emph{The International Journal of Human Resource Management}, 1-33.
#' doi: 10.1080/09585192.2020.1711798.
#' 
#' @references Lamberti, G. (2021). Hybrid multigroup partial least squares structural equation
#'  modelling: an application to bank employee satisfaction and loyalty. \emph{Quality and Quantity},
#'  doi: 10.1007/s11135-021-01096-9.
#' 
#' @keywords datasets
NULL











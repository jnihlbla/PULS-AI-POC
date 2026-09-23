//W114J099 JOB (670W1140100W114J099,W100),'RTN W114S9',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.IGRT.PROCLIB,W.QASE.PROCLIB)                            
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W114    EXEC W114P099                                                         
//*                                                                             
//*OPEND  EXEC WSOPEND,PROCESS=W114J099                                         

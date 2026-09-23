//W224J038 JOB (670W2240100W224J038,W100),'RTN W224V1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
//W224    EXEC W224P038                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W224J038                                         

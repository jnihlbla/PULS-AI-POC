//W479J038 JOB (640W4790100W479J038,W100),'RTN W479D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
/*JOBPARM FORMS=1800,LINECT=0                                                   
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W479     EXEC W479P038                                                        
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W479J038                                         

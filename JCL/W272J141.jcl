//W272J141 JOB (670W2720100W272J141,W100),'RTN W272V1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
//W272    EXEC W272P041,                                                        
//             INDIN=W272.W271V1                                                
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W272J141                                         

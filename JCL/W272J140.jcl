//W272J140 JOB (640W2720100W272J140,W100),'RTN W271V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W272    EXEC W272P040,                                                        
//             INDUT=W272.W271V1                                                
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W272J140                                         

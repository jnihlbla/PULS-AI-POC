//W272J110 JOB (640W2720100W272J110,W100),'RTN W271V2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTÖ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W272    EXEC W272P010,REFILL=V2,                                              
//             INDUT=W271.W271V2                                                
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W272J110                                         

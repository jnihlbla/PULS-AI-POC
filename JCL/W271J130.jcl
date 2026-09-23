//W271J130 JOB (640W2710100W271J130,W100),'RTN W271V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTÖ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W271    EXEC W271P030,                                                        
//             TYP=WEEK,REFILL=V2,                                              
//             INDUT=W271.W271V1                                                
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W271J130                                         

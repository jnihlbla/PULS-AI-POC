//W271D9M2 JOB (640W2710100W271D9M2,W100),'RTN W271D9',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
/*AFTER MEMOAPIX                                                                
//*---  WMEMOSND,EXC                                                            
//*                                                                             
//TOMTEST  EXEC WEMPTST,DSIN=W271.W271D9.W271351(+0)                            
//*                                                                             
//*                                                                             
//WMEMOSND EXEC WMEMOSND,CONDS='(0,LT,TOMTEST.T)',                              
//             REQS=W271D9M2                                                    
//SEND DD DSN=W271.W271D9.W271351(+0),DISP=(OLD,KEEP)                           
//     DD *                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W271D9M2                                         

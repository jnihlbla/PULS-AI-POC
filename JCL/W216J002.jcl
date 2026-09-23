//W216J002 JOB (640W2160100W216J002,W100),'RTN W216S1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W216    EXEC W216P002                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W216J002                                         

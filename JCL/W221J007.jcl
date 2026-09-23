//W221J007 JOB (640W2210100W221J007,W100),'RTN W221D6',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//WAIT    EXEC WWAIT,SECONDS=1200                                               
//W221    EXEC W221P007                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W221J007                                         

//W416J004 JOB (640W4160100W416J004,W100),'RTN W416M1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W416    EXEC W416P004                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W416J004                                         

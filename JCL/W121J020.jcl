//W121J020 JOB (640W1210100W121J020,W100),'RTN W121PV',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST1                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W121    EXEC W121P020                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W121J020                                         

//W159J003 JOB (640W1590100W159J003,W100),'RTN W159D2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTÖ                                                     
//     INCLUDE MEMBER=SYST1                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W159    EXEC W159P003                                                         
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -                 
//SOPEND  EXEC WSOPEND,PROCESS=W159J003                                         

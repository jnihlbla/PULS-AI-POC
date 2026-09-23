//W418J004 JOB (640W4180100W418J004,W100),'RTN W418D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W418    EXEC W418P004                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W418J004                                         

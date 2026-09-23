//W426J091 JOB (640W4260100W426J091,W100),'RTN W426D2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W426    EXEC W426P091                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W426J091                                         

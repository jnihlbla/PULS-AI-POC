//W513Y07B JOB (640W5130100W513Y07B,W100),'RTN W513Y1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST5                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W513    EXEC W513P07B                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W513Y07B                                         

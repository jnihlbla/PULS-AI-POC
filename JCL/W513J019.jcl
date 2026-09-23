//W513J019 JOB (640W5130100W513J019,W100),'RTN WYR001',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST5                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W513    EXEC W513P019                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W513J019                                         

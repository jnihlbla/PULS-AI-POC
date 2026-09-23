//W515J050 JOB (640W5100100W515J050,W100),'RTN W515M2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST5                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W515    EXEC W515P050                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W515J050                                         

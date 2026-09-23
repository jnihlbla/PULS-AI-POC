//W418J048 JOB (640W4180100W418J048,W100),'RTN W418D6',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST4                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W418    EXEC W418P048                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W418J048                                         

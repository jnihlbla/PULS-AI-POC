//WF10M2RE JOB (640WF100100WF10M2RE,W100),'RTN WF10M2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//SOP     EXEC WSOP                                                             
ORDER W512M4                                                                    
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WF10M2RE                                         

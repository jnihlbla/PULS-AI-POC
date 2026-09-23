//W553B4RE JOB (640W5530100W553B4RE,W100),'RTN W553B4',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//SOP     EXEC WSOP                                                             
ORDER W551B9                                                                    
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W553B4RE                                         

//W416S1RE JOB (640W4160100W416S1RE,W100),'RTN W416S1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//*                                                                             
//SOP     EXEC WSOP                                                             
ORDER W215S3                                                                    
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W416S1RE                                         

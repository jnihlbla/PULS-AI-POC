//WXTRS1RE JOB (650W0001000WXTRS1RE,W100),'RTN WXTRS1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//SOP     EXEC WSOP                                                             
ORDER W810S1                                                                    
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WXTRS1RE                                         

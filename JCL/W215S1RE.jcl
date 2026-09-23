//W215S1RE JOB (640W2150100W215S1RE,W100),'RTN W215S1',                         
//             CLASS=L,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//SOP     EXEC WSOP                                                             
ORDER W215S2                                                                    
//*                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W215S1RE                                         

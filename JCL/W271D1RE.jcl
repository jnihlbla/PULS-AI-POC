//W271D1RE JOB (640W2710100W271D1RE,W100),'RTN W271D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//SOP     EXEC WSOP                                                             
ORDER W271S1                                                                    
ORDER W271S2                                                                    
ORDER W271S4                                                                    
ORDER W271S5                                                                    
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W271D1RE                                         

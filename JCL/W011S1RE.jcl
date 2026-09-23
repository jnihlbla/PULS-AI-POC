//W011S1RE JOB (640W0110100W011S1RE,W100),'RTN W011S1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//SOP     EXEC WSOP                                                             
ORDER WXTRS1                                                                    
ORDER W217S4                                                                    
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W011S1RE                                         

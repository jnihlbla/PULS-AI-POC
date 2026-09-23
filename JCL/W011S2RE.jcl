//W011S2RE JOB (640W0110100W011S2RE,W100),'RTN W011S2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//SOP     EXEC WSOP                                                             
ORDER WXTRS2                                                                    
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W011S2RE                                         

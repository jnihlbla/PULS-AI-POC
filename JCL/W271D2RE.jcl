//W271D2RE JOB (640W2710100W271D2RE,W100),'RTN W271D2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//SOP     EXEC WSOP                                                             
ORDER W412S1 SYMBOLS                                                            
   ORDTYP(W271D2)                                                               
END-ORDER                                                                       
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W271D2RE                                         

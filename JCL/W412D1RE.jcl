//W412D1RE JOB (640W4120100W412D1RE,W100),'RTN W412D1',                         
//             CLASS=L,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//FREE    EXEC WFREE,NAME=W412D1,MAXRC=8                                        
//*                                                                             
//SOP     EXEC WSOP                                                             
ORDER W412S1 SYMBOLS                                                            
   ORDTYP(W412D1)                                                               
END-ORDER                                                                       
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W412D1RE                                         

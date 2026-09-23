//W412D4RE JOB (640W4120100W412D4RE,W100),'RTN W412D4',                         
//             CLASS=L,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//FREE  EXEC WFREE,NAME=W412D4,MAXRC=8                                          
//*                                                                             
//SOP   EXEC WSOP                                                               
ORDER W412S1 SYMBOLS                                                            
  ORDTYP(W412D4)                                                                
END-ORDER                                                                       
//*                                                                             
//EMPTYT1 EXEC WEMPTST,DSIN=W412.W412D4.W41229(+0)                              
//*                                                                             
//    IF (EMPTYT1.T.RC = 0) THEN                                                
//      EXEC WSOP                                                               
        ORDER W412D9                                                            
//    ELSE                                                                      
//      EXEC PGM=IEFBR14                                                        
//DD1   DD DSN=W412.W412D4.W41229(+0),DISP=(OLD,DELETE)                         
//    ENDIF                                                                     
//*                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W412D4RE                                         

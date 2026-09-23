//W476Z1US JOB (640W4760100W476Z1US,W100),'RTN W476V2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//* FAKTURAUPPG, AMERICAN PROFORMA                                              
//*                                                                             
//EMPTYT  EXEC WEMPTST,DSIN=W476.W476V2.W47642(+0)                              
//*                                                                             
//   IF (EMPTYT.T.RC = 4) THEN                                                  
//DEL1    EXEC PGM=IEFBR14                                                      
//DD      DD DSN=W476.W476V2.W47642(+0),DISP=(OLD,DELETE)                       
//   ELSE                                                                       
//VCOM    EXEC W016P022,VCOM=W476Z1US                                           
//W01622.W016ZZD1 DD DSN=W476.W476V2.W47642(+0),DISP=SHR,RECFM=FB               
//*                                                                             
//   ENDIF                                                                      
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W476Z1US                                         

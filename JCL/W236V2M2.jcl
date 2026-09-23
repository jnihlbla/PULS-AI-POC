//W236V2M2 JOB (650W2360100W236V2M2,W100),'RTN W236V2',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
/*JOBPARM LINES=999                                                             
/*JOBPARM FORMS=1800,LINECT=0                                                   
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
/*AFTER MEMOAPIX                                                                
//*---  WMEMOSND,EXC                                                            
//***********************************************************                   
//*      ÖVERFÖRING LISTA LEVERANSPRECISSION                *                   
//* =>   MEMO       TILL ANSKAFFARNA                        *                   
//***********************************************************                   
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W236.W236V2.W23642(+0)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
//W236    EXEC WMAILSND                                                         
//M.APIFILE  DD DSN=W236.W236V2.W23642(+0),DISP=SHR                             
//M.SEND     DD DUMMY                                                           
//    ENDIF                                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W236V2M2                                         

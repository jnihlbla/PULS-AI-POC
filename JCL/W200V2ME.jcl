//W200V2ME JOB (650W2360100W200V2ME,W100),'RTN W200V2',                         
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
//*      ÖVERFÖRING LISTA LEVERANSFÖRSENADE ARTIKLAR        *                   
//* =>   MEMO       TILL ANSKAFFARNA                        *                   
//***********************************************************                   
//*                                                                             
//W236    EXEC WMEMOSND                                                         
//M.APIFILE  DD DSN=W236.W200V2.W23663(+0),DISP=OLD                             
//M.SEND     DD DUMMY                                                           
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W200V2ME                                         

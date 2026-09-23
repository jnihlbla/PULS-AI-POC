//W971D1ER JOB (540W0000100W971D1ER,W100),'RTN W971D1',                         
//             CLASS=K                                                          
/*JOBPARM FORMS=1800,LINECT=0                                                   
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
/*AFTER MEMOAPIX                                                                
//*---  WMEMOSND,EXC                                                            
//********************************************************************          
//*                                                                  *          
//*    ==> ABNORMAL    <== FILEMON-TRANSFER.                         *          
//*    (I.E RNOTIFER RECEIVED FROM FILEMON TRANSFER)                 *          
//*                                                                  *          
//*   ACF2-LOGGAR  TILL W971                                         *          
//*                                                                  *          
//*   ÖVERFÖRING TILL RUTIN W971D1 HAR EJ GÅTT BRA.                  *          
//*   SE I ERROR-LOGGEN (F1XFVC.PROD.ERRLOG) EFTER ORSAKEN.          *          
//*   XFERID: W971D1                                                 *          
//*                                                                  *          
//*   NY ÖVERFÖRNING MÅSTE GÖRAS INNAN NÄSTA KÖRNING AV W971D1.      *          
//********************************************************************          
//*                                                                             
//W971   EXEC WMEMOSND                                                          
//M.APIFILE  DD DSN=W.QASE.CONSTANT(W971ME),DISP=SHR                            
//M.SEND     DD DUMMY                                                           

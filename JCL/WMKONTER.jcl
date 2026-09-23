//WMKONTER JOB (540W0000000WMKONTER,W100),'RTN WMKONT',                         
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
//*   ÖVERFÖRING TILL RUTIN WMKONT HAR EJ GÅTT BRA.                  *          
//*   SE I ERROR-LOGGEN (F1XFVC.PROD.ERRLOG) EFTER ORSAKEN.          *          
//*   XFERID: WMKONT                                                 *          
//*                                                                  *          
//********************************************************************          
//VRCABE  EXEC VRCABEND                                                         
//*                                                                             
//*                                                                             
//MEMO    EXEC WMEMOSND                                                         
//APIFILE  DD  *                                                                
)SEND                                                                           
 OPTION FORCE                                                                   
 TITLE WMKONTO FEL                                                              
 DEST  ANNA-KARIN.LOWERDAHL(A)VOLVO.COM                                         
 MEMO                                                                           
                                                                                
 FILEMON-SÄNDNING TILL DESTINATION WMKONTO (VCCS) HAR                           
 GÅTT SNETT.                                                                    
                                                                                
 VID PROBLEM KONTAKTA PULS HELPDESK 52100.                                      
)END                                                                            
//*                                                                             

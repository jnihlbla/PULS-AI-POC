//W33520ER JOB (540W3350100W33520ER,W100),'RTN W335D2',                         
//   CLASS=K                                                                    
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//********************************************************************          
//*                                                                  *          
//*    ==> ABNORMAL    <== FILEMON-TRANSFER.                         *          
//*    (I.E RNOTIFER RECEIVED FROM FILEMON TRANSFER)                 *          
//*                                                                  *          
//*   RDE/RVS-ÖVERFÖRING AV FIL W33520 FRÅN XBMS TILL RUTIN W335D2   *          
//*                HAR EJ GÅTT BRA.                                  *          
//*   SE I ERROR-LOGGEN (F1XFVC.PROD.ERRLOG) EFTER ORSAKEN.          *          
//*   XFERID: W335D2                                                 *          
//*                                                                  *          
//********************************************************************          
//SOP1    EXEC WSOP,COMMAND='ACTIVATE W33520ER'                                 
//*                                                                             
//VRCABE  EXEC VRCABEND                                                         
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W33520ER                                         
/*                                                                              

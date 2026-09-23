//W33092ER JOB (540W3300100W33092ER,W100),'RTN W330B6',                         
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
//*   RDE/RVS-ÖVERFÖRING AV FIL W33092 FRÅN XBMS TILL RUTIN W330B6   *          
//*                HAR EJ GÅTT BRA.                                  *          
//*   SE I ERROR-LOGGEN (F1XFVC.PROD.ERRLOG) EFTER ORSAKEN.          *          
//*   XFERID: W330B6                                                 *          
//*                                                                  *          
//********************************************************************          
//SOP1    EXEC WSOP,COMMAND='ACTIVATE W33092ER'                                 
//*                                                                             
//VRCABE  EXEC VRCABEND                                                         
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W33092ER                                         
/*                                                                              

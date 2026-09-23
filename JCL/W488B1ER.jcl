//W488B1ER JOB (540W4880100W488B1ER,W100),'RTN W488B1',                         
//   CLASS=K                                                                    
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//********************************************************************          
//*                                                                  *          
//*    ==> ABNORMAL    <== FILEMON-TRANSFER.                         *          
//*    (I.E SNOTIFER RECEIVED FROM FILEMON TRANSFER)                 *          
//*                                                                  *          
//*   RDE-ÖVERFÖRING FRÅN PDP:N TILL RUTIN W488B1 HAR EJ GÅTT BRA.   *          
//*   SE I ERROR-LOGGEN (F1XFVC.PROD.ERRLOG) EFTER ORSAKEN.          *          
//*   XFERID: W488B1                                                 *          
//*                                                                  *          
//********************************************************************          
//VRCABE  EXEC VRCABEND                                                         

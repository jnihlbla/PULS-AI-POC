//W09206ER JOB (540W0920100W09206ER,W100),'RTN W092D1',                         
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
//*       ÖVERFÖRING AV ORDER FRÅN PV        HAR EJ GÅTT BRA.        *          
//*   SE I ERROR-LOGGEN (F1XFVC.PROD.ERRLOG) EFTER ORSAKEN.          *          
//*   XFERID: W092D1                                                 *          
//*                                                                  *          
//********************************************************************          
//SOP1    EXEC WSOP,COMMAND='ACTIVATE W09206ER'                                 
//*                                                                             
//VRCABE  EXEC VRCABEND                                                         
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W09206ER                                         
/*                                                                              

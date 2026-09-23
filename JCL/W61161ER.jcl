//W61161ER JOB (540W6110100W61161ER,W100),'RTN W611S5',                         
//             CLASS=K                                                          
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
//*   FILEMON-ÖVERFÖRINGEN TILL V2:AN I W61161 HAR EJ GÅTT BRA.      *          
//*   SE I ERROR-LOGGEN (F1XFVC.PROD.ERRLOG I V1:AN ELLER            *          
//*   F1XFV2.QASE.ERRLOG I V2:AN) EFTER ORSAKEN.                     *          
//*   XFERID: W61161                                                 *          
//*                                                                  *          
//********************************************************************          
//SOP     EXEC WSOP                                                             
ABEND W61161FI                                                                  

//W611S5ER JOB (540W6110100W611S5ER,W100),'RTN W611S5',                         
//             CLASS=K                                                          
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
//*   FILEMON-ÖVERFÖRINGEN FRÅN PDP:N VIA V2-NODEN HAR EJ GÅTT BRA.  *          
//*   SE I ERROR-LOGGEN (F1XFVC.PROD.ERRLOG I V1:AN ELLER            *          
//*   F1XFV2.QASE.ERRLOG I V2:AN) EFTER ORSAKEN.                     *          
//*   XFERID: W611S5                                                 *          
//*   TITTA ÄVEN I JCLSPLIT PÅ JOB W611J060 SOM STARTAR FILEMON-     *          
//*   ÖVERFÖRINGEN FRÅN V2:AN.                                       *          
//*                                                                  *          
//********************************************************************          
//SOP1    EXEC WSOP,COMMAND='ACTIVATE W611S5ER'                                 
//*                                                                             
//VRCABE  EXEC VRCABEND                                                         
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W611S5ER                                         
//*                                                                             

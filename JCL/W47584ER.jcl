//W47584ER JOB (540W4750100W47584ER,W100),'RTN W475D2',                         
//        CLASS=K                                                               
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM LINES=99,FORMS=1800,LINECT=0                                          
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//********************************************************************          
//*                                                                  *          
//*    ==> ABNORMAL    <== FILEMON-TRANSFER.                         *          
//*    (I.E SNOTIFER RECEIVED FROM FILEMON TRANSFER)                 *          
//*    LOOK FOR REASON IN USER-LOGG: F1XFVC.PROD.ERRLOG(+0)          *          
//*                                                                  *          
//*   JOB W47584FI HAR EJ GÅTT TILL NORMALT SLUT                     *          
//*                                                                  *          
//********************************************************************          
//SOP     EXEC WSOP,COMMAND='ABEND W47584FI'                                    

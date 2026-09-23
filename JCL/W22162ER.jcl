//W22162ER JOB (540W2210100W22162ER,W100),'RTN W221D2',                         
//   CLASS=K                                                                    
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM LINES=99,FORMS=1800,LINECT=0                                          
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//********************************************************************          
//*                                                                  *          
//*    ==> ABNORMAL    <== FILEMON-TRANSFER.                         *          
//*    (I.E SNOTIFER RECEIVED FROM FILEMON TRANSFER)                 *          
//*    LOOK FOR REASON IN USER-LOGG: F1XFVC.PROD.ERRLOG(+0)          *          
//*                                                                  *          
//*   JOB W22162FI HAR EJ GÅTT TLL NORMALT SLUT                      *          
//*   JOBBET SKICKAR W22162 TILL V2:AN                               *          
//*                                                                  *          
//********************************************************************          
//SOP     EXEC WSOP,COMMAND='ABEND W22162FI'                                    

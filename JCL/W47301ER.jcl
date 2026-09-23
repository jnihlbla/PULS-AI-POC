//W47301ER JOB (540W4730100W47301ER,W100),'RTN W473S1',                         
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
//*   JOB W47301FI HAR EJ GÅTT TLL NORMALT SLUT                      *          
//*                                                                  *          
//********************************************************************          
//SOP     EXEC WSOP,COMMAND='ABEND W47301FI'                                    
/*                                                                              

//W01613ME JOB (540W0010300W016J013,W100),'RTN W01613',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
/*JOBPARM LINES=9,CARDS=0,FORMS=1800                                            
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
/*AFTER MEMOAPIX                                                                
//*---  WMEMOSND,EXC                                                            
//*                                                                             
//*                                                                             
//********************************************************************          
//*   SYSTEM    VCOM RECEIVING FILES.                                *          
//*   FUNKTION  THIS JCL WILL BE USED IF PGM R01613 ABENDS           *          
//*                                                                  *          
//********************************************************************          
//MEMO     EXEC WMEMOSND                                                        
)SEND                                                                           
 TITLE W01613 ABEND                                                             
 DEST WSYST@VOLVOCARS.COM                                                       
 OPTION FORCE                                                                   
 MEMO                                                                           
VCOM RECEIVING PGM W01613 HAS ABENDED !                                         

//WXTRJ09B JOB (640WXTR0100WXTRJ09B,W100),'RTN WXTRD2',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYSTÖ                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
//WXTR    EXEC WXTRP09B                                                         
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=WXTR.WXTRD2.WXTR9B(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP4,DSIN=WXTR.WXTRD2.WXTR9B(+1),CPU=3                              
//* SYSIN USED TO IGNORE THE CONTROL CHARACTERS IN VBA FILE                     
//SYSIN DD *                                                                    
1                                                                               
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WXTRJ09B                                         

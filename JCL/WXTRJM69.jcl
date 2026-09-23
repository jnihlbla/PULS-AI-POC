//WXTRJM69 JOB (640W0001000WXTRJM69,W100),'RTN WXTRV2',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTZ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND VCC1                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//* Sends mail with attached file, created in previous job WXTRJ069             
//* WXTR.WXTRV2.WXTR69(+0)                                                      
//* A new Warehouse Address Diff-list is produced                               
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=WXTR.WXTRV2.WXTR69(+0)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=WXTR.WXTRV2.WXTR69(+0)                                    
//SYSIN           DD *                                                          
WXTR69-001                                                                      
WXTR69                                                                          
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WXTRJM69                                         

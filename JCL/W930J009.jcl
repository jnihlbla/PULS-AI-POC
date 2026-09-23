//W930J009 JOB (640W9300100W930J009,W100),'RTN W930S3',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST9                                                     
//     INCLUDE MEMBER=SYSTZ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*+JBS BIND IMG0                                                               
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W930.W930S3.W93007(+0)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W930.W930S3.W93007(+0)                                    
//SYSIN           DD *                                                          
W93009-001                                                                      
W93009-1                                                                        
//    ENDIF                                                                     
//EMPTY2 EXEC WEMPTST,DSIN=W930.W930S3.W93008(+0)                               
//    IF (EMPTY2.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W930.W930S3.W93008(+0)                                    
//SYSIN           DD *                                                          
W93009-002                                                                      
W93009-2                                                                        
//    ENDIF                                                                     
//SOPEND  EXEC WSOPEND,PROCESS=W930J009                                         

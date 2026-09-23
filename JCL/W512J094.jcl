//W512J094 JOB (640W5120100W512J094,W100),'RTN W512M2',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST5                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W512    EXEC W512P094                                                         
//W51294.SYSUDUMP DD SYSOUT=*                                                   
//W51294.IDIREPRT DD SYSOUT=(A,,SYST)                                           
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W512.W512M2.W51293A(+1)                              
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP4,DSIN=W512.W512M2.W51293A(+1),CPU=10                            
//* SYSIN USED TO IGNORE THE CONTROL CHARACTERS IN VBA FILE                     
//SYSIN DD *                                                                    
1                                                                               
//    ENDIF                                                                     
//*                                                                             
//EMPTY2 EXEC WEMPTST,DSIN=W512.W512M2.W51294A(+1)                              
//    IF (EMPTY2.T.RC = 0) THEN                                                 
// EXEC WZ14DAP4,DSIN=W512.W512M2.W51294A(+1),CPU=10                            
//* SYSIN USED TO IGNORE THE CONTROL CHARACTERS IN VBA FILE                     
//SYSIN DD *                                                                    
1                                                                               
//    ENDIF                                                                     
//*                                                                             
//EMPTY3 EXEC WEMPTST,DSIN=W512.W512M2.W51295A(+1)                              
//    IF (EMPTY3.T.RC = 0) THEN                                                 
// EXEC WZ14DAP4,DSIN=W512.W512M2.W51295A(+1),CPU=10                            
//* SYSIN USED TO IGNORE THE CONTROL CHARACTERS IN VBA FILE                     
//SYSIN DD *                                                                    
1                                                                               
//    ENDIF                                                                     
//*                                                                             
//EMPTY4 EXEC WEMPTST,DSIN=W512.W512M2.W51296A(+1)                              
//    IF (EMPTY4.T.RC = 0) THEN                                                 
// EXEC WZ14DAP4,DSIN=W512.W512M2.W51296A(+1),CPU=10                            
//* SYSIN USED TO IGNORE THE CONTROL CHARACTERS IN VBA FILE                     
//SYSIN DD *                                                                    
1                                                                               
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W512J094                                         

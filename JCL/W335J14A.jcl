//W335J14A JOB (670W3350100W335J14A,W100),'RTN W335B2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST1                                                     
//     INCLUDE MEMBER=SYST3                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W335     EXEC W335P04A,                                                       
//             TYP=&MKOMP,                                                      
//             DSIN=W335.W335B2.W33540A(+0),                                    
//             DSUT=W335.W335B2.W33540(+1)                                      
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W335J14A                                         

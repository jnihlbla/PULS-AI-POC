//W335J24B JOB (670W3350100W335J24B,W100),'RTN W335D7',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST1                                                     
//     INCLUDE MEMBER=SYST3                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W335     EXEC W335P24B,                                                       
//            DSIN=W335.W335D7.W33540A(+0),                                     
//            DSUT=W335.W335D7                                                  
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W335J24B                                         

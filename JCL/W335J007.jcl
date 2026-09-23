//W335J007 JOB (640W3350100W335J005,W100),'RTN W335X4',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST3                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W335    EXEC W335P007,                                                        
//             INDIN=W335.&VCOM,                                                
//             INDUT=W335.&VCOM                                                 
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W335J007                                         

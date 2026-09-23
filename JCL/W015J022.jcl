//W015J022 JOB (640W0020200W015J022,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST0                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//        EXEC W001ISPF                                                         
//TSO.SYSTSPRT DD DSN=W015.W010V9.W01521(+1),                                   
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(137,(12,10),RLSE),AVGREC=K,                               
//             MGMTCLAS=NOBACKUP                                                
//TSO0.SYSTSIN DD DSN=W.QASE.CONSTANT(W015IXV1),DISP=SHR                        
//*                                                                             
//W015    EXEC W015P022                                                         
//W01522.W01522D1 DD DSN=W015.W010V9.W01521(+1)                                 
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W015J022                                         
//*                                                                             

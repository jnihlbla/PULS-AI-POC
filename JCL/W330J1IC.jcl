//W330J1IC JOB (640W3300100W330J1IC,W100),'RTN W330R1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTÖ                                                     
//     INCLUDE MEMBER=SYST3                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W330    EXEC W330P1IC                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W330J1IC                                         

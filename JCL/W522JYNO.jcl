//W522JYNO JOB (640W5220100W522JYNO,W100),'RTN W522M1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST5                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W522    EXEC W522P022                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W522JYNO                                         

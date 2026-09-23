//W522J1PL JOB (640W5220100W522J1PL,W100),'RTN W522M1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=DESTN                                                     
//     INCLUDE MEMBER=SYST5                                                     
/*JOBPARM FORMS=1800,LINECT=0,LINES=999                                         
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W522    EXEC W522P114,                                                        
//             SOUT1='(A,,INTO)'                                                
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W522J1PL                                         

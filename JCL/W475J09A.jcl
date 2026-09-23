//W475J09A JOB (640W4750100W475J09A,W100),'RTN W475M0',                         
//             CLASS=K,                                                         
//             USER=?,PASSWORD=?                                                
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTÖ                                                     
//     INCLUDE MEMBER=SYST5                                                     
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W475    EXEC W475P09A                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W475J09A                                         

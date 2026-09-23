//W463J05A JOB (640W4630100W463J05A,W100),'RTN W463S9',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//W463    EXEC W463P05A                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W463J05A                                         

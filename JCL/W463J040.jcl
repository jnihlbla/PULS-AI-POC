//W463J040 JOB (670W4630100W463J040,W100),'RTN W463S4',                         
//             CLASS=L,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W463    EXEC W463P040                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W463J040                                         

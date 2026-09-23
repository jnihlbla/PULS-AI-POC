//W463J055 JOB (670W4630100W463J055,W100),'RTN W463S9',                         
//             CLASS=L,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W463    EXEC W463P055                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W463J055                                         

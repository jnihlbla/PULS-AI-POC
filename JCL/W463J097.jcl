//W463J097 JOB (640W4630100W463J097,W100),'RTN W463D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC JCLLIB ORDER=(W.QASE.PROCLIB)                                            
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*                                                                             
//W463     EXEC W463P097                                                        
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W463J097                                         

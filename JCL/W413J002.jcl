//W413J002 JOB (640W4130100W413J002,W100),'RTN W413D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//WAIT    EXEC WWAIT,SECONDS=700                                                
//*                                                                             
//W413    EXEC W413P002                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W413J002                                         

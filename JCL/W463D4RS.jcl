//W463D4RS JOB (640W4630100W463D4RS,W100),'RTN W463D4',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//BLOCK   EXEC WBLOCK,NAME=W463D4                                               
//*                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W463D4RS                                         

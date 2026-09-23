//W116D5RS JOB (640W1160100W116D5RS,W100),'RTN W116D5',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//BLOCK   EXEC WBLOCK,NAME=W116D5                                               
//*                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W116D5RS                                         

//W560M1RS JOB (640W5600100W560M1RS,W100),'RTN W560M1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//BLOCK   EXEC WBLOCK,NAME=W560M1                                               
//*                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W560M1RS                                         

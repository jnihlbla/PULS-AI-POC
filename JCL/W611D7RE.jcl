//W611D7RE JOB (640W6110100W611D7RE,W100),'RTN W611D7',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//FREE    EXEC WFREE,NAME=W611D7,MAXRC=8                                        
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W611D7RE                                         

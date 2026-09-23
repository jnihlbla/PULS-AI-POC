//W426V2RE JOB (640W4260100W426V2RE,W100),'RTN W426V2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//FREE    EXEC WFREE,NAME=W426V2,MAXRC=8                                        
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W426V2RE                                         

//WF10D2RE JOB (640WF100100WF10D2RE,W100),'RTN WF10D2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//FREE    EXEC WFREE,NAME=WF10D2,MAXRC=8                                        
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WF10D2RE                                         

//WXTRV3RE JOB (650W0001000WXTRV3RE,W100),'RTN WXTRV3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//FREE    EXEC WFREE,NAME=WXTRV3,MAXRC=8                                        
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WXTRV3RE                                         

//WF10B2RE JOB (640WF100100WF10B2RE,W100),'RTN WF10B2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//FREE    EXEC WFREE,NAME=WF10B2,MAXRC=8                                        
//*                                                                             
//SOP     EXEC WSOP,COMMAND='ORDER W930B1'                                      
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WF10B2RE                                         

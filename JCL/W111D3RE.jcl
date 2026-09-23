//W111D3RE JOB (650W1110100W111D3RE,W100),'RTN W111D3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//FREE    EXEC WFREE,NAME=W111D3,MAXRC=8                                        
//SOP     EXEC WSOPEND,PROCESS=W111D3RE                                         

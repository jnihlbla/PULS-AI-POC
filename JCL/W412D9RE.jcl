//W412D9RE JOB (640W4120100W412D9RE,W100),'RTN W412D9',                         
//             CLASS=L,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//FREE  EXEC WFREE,NAME=W412D9,MAXRC=8                                          
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W412D9RE                                         

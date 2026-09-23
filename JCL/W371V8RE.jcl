//W371V8RE JOB (650W3710100W371V8RE,W100),'RTN W371V8',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//FREE    EXEC WFREE,NAME=W371V8,MAXRC=8                                        
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W371V8RE                                         

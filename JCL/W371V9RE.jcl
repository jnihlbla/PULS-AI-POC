//W371V8RE JOB (650W3710100W371V9RE,W100),'RTN W371V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//FREE    EXEC WFREE,NAME=W371V9,MAXRC=8                                        
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W371V9RE                                         

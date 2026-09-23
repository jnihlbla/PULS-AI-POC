//W371V2RE JOB (650W3710100W371V2RE,W100),'RTN W371V2',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//FREE    EXEC WFREE,NAME=W371V2,MAXRC=8                                        
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W371V2RE                                         

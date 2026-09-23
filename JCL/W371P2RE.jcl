//W371P2RE JOB (650W3710100W371P2RE,W100),'RTN W371P2',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//FREE    EXEC WFREE,NAME=W371P2,MAXRC=8                                        
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W371P2RE                                         

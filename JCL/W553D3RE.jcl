//W553D3RE JOB (650W5530100W553D3RE,W100),'RTN W553D3',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//FREE    EXEC WFREE,NAME=W553D3,MAXRC=8                                        
//SOP     EXEC WSOPEND,PROCESS=W553D3RE                                         

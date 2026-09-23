//W114D2RE JOB (640W1140100W114D2RE,W100),'RTN W114D2',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//FREE    EXEC WFREE,NAME=W114D2,MAXRC=8                                        
//SOP     EXEC WSOPEND,PROCESS=W114D2RE                                         

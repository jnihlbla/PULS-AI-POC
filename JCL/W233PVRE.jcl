//W233PVRE JOB (650W2330100W233PVRE,W100),'RTN W233PV',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//FREE    EXEC WFREE,NAME=W233PV,MAXRC=8                                        
//SOP     EXEC WSOPEND,PROCESS=W233PVRE                                         

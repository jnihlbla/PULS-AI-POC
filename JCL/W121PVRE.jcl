//W121PVRE JOB (650W1210100W121PVRE,W100),'RTN W121PV',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//FREE  EXEC WFREE,NAME=W121PV,MAXRC=8                                          
//SOP     EXEC WSOPEND,PROCESS=W121PVRE                                         

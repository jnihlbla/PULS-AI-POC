//W122B2RE JOB (650W0010300W122B2RE,W100),'RTN W122B2',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//FREE    EXEC WFREE,NAME=W122B2,MAXRC=8                                        
//SOP     EXEC WSOPEND,PROCESS=W122B2RE                                         

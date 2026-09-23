//W200V2RE JOB (650W0010300W200V2RE,W100),'RTN W200V2',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//FREE    EXEC WFREE,NAME=W200V2,MAXRC=8                                        
//SOP     EXEC WSOPEND,PROCESS=W200V2RE                                         

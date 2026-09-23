//W371D5RE JOB (650W3710100W371D5RE,W100),'RTN W371D5',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//FREE    EXEC WFREE,NAME=W371D5,MAXRC=8                                        
//SOP     EXEC WSOPEND,PROCESS=W371D5RE                                         

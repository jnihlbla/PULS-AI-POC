//W330D5RE JOB (650W3300100W330D5RE,W100),'RTN W330D5',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800                                                            
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//FREE    EXEC WFREE,NAME=W330D5,MAXRC=8                                        
//SOP     EXEC WSOPEND,PROCESS=W330D5RE                                         

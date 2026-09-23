//W330V1RE JOB (650W3300100W330V1RE,W100),'RTN W330V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800                                                            
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//FREE    EXEC WFREE,NAME=W330V1,MAXRC=8                                        
//SOP     EXEC WSOPEND,PROCESS=W330V1RE                                         

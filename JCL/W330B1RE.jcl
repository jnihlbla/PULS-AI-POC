//W330B1RE JOB (650W3300100W330B1RE,W100),'RTN W330B1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//FREE    EXEC WFREE,NAME=W330B1,MAXRC=8                                        
//SOP     EXEC WSOPEND,PROCESS=W330B1RE                                         

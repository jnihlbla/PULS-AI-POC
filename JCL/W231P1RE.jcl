//W231P1RE JOB (650W2310100W231P1RE,W100),'RTN W231P1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//FREE    EXEC WFREE,NAME=W231P1,MAXRC=8                                        
//SOP     EXEC WSOPEND,PROCESS=W231P1RE                                         

//W477M1RE JOB (650W4750100W477M1RE,W100),'RTN W477M1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//FREE    EXEC WFREE,NAME=W477M1,MAXRC=8                                        
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W477M1RE                                         

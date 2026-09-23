//W331M9RE JOB (650W0010300W331M9RE,W100),'RTN W331M9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=00                                                  
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//FREE    EXEC WFREE,NAME=W331M9,MAXRC=8                                        
//SOP     EXEC WSOPEND,PROCESS=W331M9RE                                         

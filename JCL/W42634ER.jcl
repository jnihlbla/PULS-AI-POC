//W42634ER JOB (640W4260100W42634ER,W100),'RTN W426V1',                         
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//ABEND   EXEC VRCABEND                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W42634FI                                         

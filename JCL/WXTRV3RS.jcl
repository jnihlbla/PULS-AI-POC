//WXTRV3RS JOB (650W0001000WXTRV3RS,W100),'RTN WXTRV3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//BLOCK   EXEC WBLOCK,NAME=WXTRV3                                               
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WXTRV3RS                                         
